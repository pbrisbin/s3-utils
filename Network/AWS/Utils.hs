module Network.AWS.Utils
    ( 
    -- * Types
      Bucket
    , Local(..)
    , Remote(..)
    , Arg(..)

    -- * Actions
    , putFile
    , getFile
    , getDirectory

    -- * Helpers
    , parseArg
    , allSameType
    , mapDirectory

    -- * Remote helpers
    , remoteIsDirectory
    , remoteListDirectory

    -- * Error messages
    , errorInvalidArgs
    , errorEnvNotSet
    ) where

import Network.AWS.AWSConnection
import Network.AWS.AWSResult
import Network.AWS.S3Object
import Network.AWS.S3Bucket
import Network.Wai.Application.Static (defaultMimeTypeByExt)

import Control.Exception  (IOException, handle)
import Control.Monad      (forM_, when, unless)
import Data.List          (isPrefixOf)
import System.FilePath    (splitFileName, (</>))
import System.IO          (hPutStrLn, stderr)

import System.Directory

import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8      as C8

type Bucket = String

-- | A file or directory on the local system
data Local = Local
    { filePath :: FilePath
    }

-- | A file \"in the cloud\"
data Remote = Remote
    { bucket :: Bucket
    , path   :: FilePath
    }

-- | An argument to a file processing command, can be local or remote
data Arg = L Local | R Remote

-- | Uploads the local file to the remote location, prints any errors to 
--   stdout
putFile :: AWSConnection -> Local -> Remote -> IO ()
putFile aws local@(Local fpFrom) (Remote b "") = do
    -- an empty bucket path should put basename of the from file into 
    -- the top level of the bucket
    let fpTo = baseName fpFrom
    putFile aws local (Remote b fpTo)

putFile aws (Local fpFrom) (Remote b fpTo) = handle skip $ do
    obj <- do
        fileData <- B.readFile fpFrom
        return S3Object
            { obj_bucket   = b
            , obj_name     = fpTo
            , content_type = C8.unpack $ defaultMimeTypeByExt fpFrom
            , obj_headers  = []
            , obj_data     = fileData
            }
    resp <- sendObject aws obj

    case resp of
        Left e  -> hPutStrLn stderr $ show e
        Right _ -> return ()
    
-- | Downloads the remote file to the local location, prints any errors 
--   to stdout
getFile :: AWSConnection -> Remote -> Local -> IO ()
getFile aws (Remote b fpFrom) (Local fpTo) = handle skip $ do
    --
    -- handle the case where fpTo is a directory
    --
    -- s3cp b:etc/X11/xorg.conf ./
    --
    -- should write ./xorg.conf
    --
    fpTo' <- do
        isDirectory <- doesDirectoryExist fpTo
        if isDirectory
            then return $ fpTo </> baseName fpFrom
            else return fpTo

    --
    -- handle the case of fpTo being a long path
    --
    -- s3cp b:etc/X11/xorg.conf ./etc.bak/X11/xorg.conf
    --
    -- should create all leading paths and write the file there
    --
    let (dir,_) = splitFileName fpTo
    unless (null dir) $ createDirectoryIfMissing True dir

    -- a skeleton object, identifies the file by bucket/path
    let obj = S3Object b fpFrom "" [] (L8.pack "")
    resp <- getObject aws obj
    case resp of
        Left e     -> hPutStrLn stderr $ show e
        Right obj' -> B.writeFile fpTo' (obj_data obj')

-- | Same but acts recursively on a directory
getDirectory :: AWSConnection -> Remote -> Local -> IO ()
getDirectory aws r@(Remote b fpFrom) (Local fpTo) = handle skip $ do
    --
    -- copying a directory to a file is invalid usage
    --
    isFile <- doesFileExist fpTo
    when isFile $ hPutStrLn stderr errorInvalidArgs

    --
    -- handle the case were fpTo is a directory that does not exit
    --
    -- s3cp b:etc ./etc.bak
    --
    -- should create ./etc.bak then copy b:etc/* to ./etc.bak/
    --
    createDirectoryIfMissing True fpTo

    --
    -- handle the case were fpTo is a directory that exists
    --
    -- s3cp b:etc/X11 /etc
    --
    -- should copy b:etc/X11/* into /etc/X11/
    --
    -- FIXME
    --
    remotes <- remoteListDirectory aws r

    forM_ remotes $ \remote -> do
        let dst = fpTo </> fixFileName remote
        getFile aws remote (Local dst)

    where
        fixFileName :: Remote -> FilePath
        fixFileName = stripLeadingSlash . stripPrefix fpFrom . path

        stripPrefix :: String -> String -> String 
        stripPrefix pref str =
            if pref `isPrefixOf` str
                then drop (length pref) str
                else str


-- | Test if the remote argument is what we would consider a directory. 
--   Beware sketchy algorithm ahead...
remoteIsDirectory :: AWSConnection -> Remote -> IO Bool
remoteIsDirectory aws (Remote b fp) = do
    let req = ListRequest (addTrailingSlash fp) "" "" 1
    resp <- listObjects aws b req
    case resp of
        Left  _            -> return False
        Right (_, [])      -> return False
        Right (_, results) -> return True

-- | List the contents of a remote directory
remoteListDirectory :: AWSConnection -> Remote -> IO [Remote]
remoteListDirectory aws remote@(Remote b fp) = do
    results <- listDirectory "" aws remote
    return $ map (\res -> Remote b (key res)) results

listDirectory :: String -> AWSConnection -> Remote -> IO [ListResult]
listDirectory marker aws remote@(Remote b fp) = do
    let req = ListRequest (addTrailingSlash fp) marker "" 1000
    resp <- listObjects aws b req
    case resp of
        Left e -> do
            hPutStrLn stderr $ show e
            return []

        -- TODO: empty or directory not found?
        Right (_,     []     ) -> return []
        Right (trunc, thisSet) -> do
            if trunc
                then do
                    let lastItem = key $ last thisSet
                    nextSet <- listDirectory lastItem aws remote
                    return $ init thisSet ++ nextSet
                else return thisSet

-- | Recursively execute a function on all files in the passed directory
mapDirectory :: FilePath -> (FilePath -> IO ()) -> IO ()
mapDirectory dir f = handle skip $ do
    names <- getDirectoryContents dir
    let pnames = filter (`notElem` [".", ".."]) names
    forM_ pnames $ \name -> do
        let path = dir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory then mapDirectory path f else f path

-- | Presense of a colon means remote arg, break it; else, it's a local 
--   filepath. TODO: be smarter about that.
parseArg :: String -> Arg
parseArg arg = let bucket = takeWhile (/= ':') arg
                   len = length bucket
               in if len == length arg
                    then L $ Local arg -- local filepath
                    else R . Remote bucket . stripLeadingSlash $ drop (len+1) arg

-- | Validate that a list of arguments are all local or all remote
allSameType :: [Arg] -> Bool
allSameType []  = True
allSameType [_] = True
allSameType (a:b:rest) = a `sameAs` b && allSameType (b:rest)

sameAs :: Arg -> Arg -> Bool
sameAs (L _) (L _) = True
sameAs (R _) (R _) = True
sameAs _     _     = False

baseName :: FilePath -> FilePath
baseName = snd . splitFileName

addTrailingSlash :: FilePath -> FilePath
addTrailingSlash = reverse . addSlash . reverse

    where
        addSlash :: FilePath -> FilePath
        addSlash s@('/':_) = s
        addSlash s         = '/':s

removeTrailingSlash :: String -> String
removeTrailingSlash = reverse . stripLeadingSlash . reverse

stripLeadingSlash :: String -> String
stripLeadingSlash ('/':rest) = stripLeadingSlash rest
stripLeadingSlash x          = x

skip :: IOException -> IO ()
skip e = hPutStrLn stderr $ show e

-- | Invalid arguments for operation
errorInvalidArgs :: String
errorInvalidArgs = "Invalid arguments for operation"

-- | AWS environment variables are not set
errorEnvNotSet :: String
errorEnvNotSet = "AWS environment variables are not set"
