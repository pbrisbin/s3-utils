module Network.AWS.Utils
    ( 
    -- * Types
      Bucket
    , Local(..)
    , Remote(..)
    , Arg(..)

    -- * Actions
    , putFile
    , putDirectory
    , getFile
    , getDirectory
    , listDirectory

    -- * Argument helpers
    , parseArg
    , allSame
    , allLocal
    , allRemote
    , sameAs

    -- * Remote helpers
    , remoteIsDirectory
    , remoteListDirectory

    -- * Error messages
    , errorInvalidArgs
    , errorEnvNotSet
    ) where

import Network.AWS.AWSConnection
import Network.AWS.S3Object
import Network.AWS.S3Bucket
import Network.Wai.Application.Static (defaultMimeTypeByExt)

import Control.Exception  (IOException, handle)
import Control.Monad      (forM_, when)
import Data.List          (isPrefixOf)
import System.FilePath    (splitFileName, (</>))
import System.IO          (hPutStrLn, stderr)

import System.Directory

import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8      as C8

type Bucket = String

-- | A file or directory on the local system
data Local = Local { filePath :: FilePath }

-- | A file \"in the cloud\"
data Remote = Remote
    { bucket :: Bucket
    , path   :: FilePath
    }

-- | Any argument to a file processing command
data Arg = L Local | R Remote

putFile :: AWSConnection
        -> Local  -- ^ known to be a file
        -> Remote -- ^ existing directory or new filename
        -> IO ()
putFile aws l@(Local fpFrom) (Remote b "") = do
    -- an empty bucket path should place file at top level
    let fpTo = baseName fpFrom
    putFile aws l (Remote b fpTo)

putFile aws (Local fpFrom) r@(Remote b fpTo) = handle skip $ do
    fpTo' <- do
        isDirectory <- remoteIsDirectory aws r
        if isDirectory
            then return $ fpTo </> baseName fpFrom
            else return fpTo

    obj <- do
        fileData <- B.readFile fpFrom
        return S3Object
            { obj_bucket   = b
            , obj_name     = fpTo'
            , content_type = C8.unpack $ defaultMimeTypeByExt fpFrom
            , obj_headers  = []
            , obj_data     = fileData
            }

    resp <- sendObject aws obj
    case resp of
        Left e  -> hPutStrLn stderr $ show e
        Right _ -> return ()
    
getFile :: AWSConnection
        -> Remote -- ^ known to be a file
        -> Local  -- ^ existing directory or new filename
        -> IO ()
getFile aws (Remote b fpFrom) (Local fpTo) = handle skip $ do
    fpTo' <- do
        isDirectory <- doesDirectoryExist fpTo
        if isDirectory
            then return $ fpTo </> baseName fpFrom
            else return fpTo

    resp <- getObject aws $ S3Object b fpFrom "" [] (L8.pack "")
    case resp of
        Left e     -> hPutStrLn stderr $ show e
        Right obj' -> B.writeFile fpTo' (obj_data obj')

getDirectory :: AWSConnection
             -> Remote -- ^ known to be a directory
             -> Local  -- ^ interpreted as a directory, can exist or not
             -> IO ()
getDirectory aws r@(Remote _ fpFrom) (Local fpTo) = handle skip $ do
    -- copying a directory to a file is invalid usage
    isFile <- doesFileExist fpTo
    when isFile $
        hPutStrLn stderr errorInvalidArgs

    -- directory exists
    isDirectory <- doesDirectoryExist fpTo
    when isDirectory $ do
        remotes <- remoteListDirectory aws r
        forM_ remotes $ \remote ->
            go aws remote $ fpTo </> path remote
        
    -- directory does not exist
    remotes <- remoteListDirectory aws r
    forM_ remotes $ \remote ->
        go aws remote $ fpTo </> stripLeadingSlash (stripPrefix fpFrom fpTo)

    where
        -- factor common logic
        go ::  AWSConnection -> Remote -> FilePath -> IO ()
        go aws' remote' dst' = do
            let (dir,_) = splitFileName dst'
            createDirectoryIfMissing True dir
            getFile aws' remote' (Local dst')

putDirectory :: AWSConnection
             -> Local  -- ^ known to be a directory
             -> Remote -- ^ interpreted as a directory, can exist or not
             -> IO ()
putDirectory aws l@(Local fpFrom) (Remote b "") = do
    -- empty bucket should place the directory at top level
    let fpTo = baseName fpFrom
    putDirectory aws l (Remote b fpTo)
    
putDirectory aws (Local fpFrom) r@(Remote b fpTo) = do
    -- directory exists
    isDirectory <- remoteIsDirectory aws r
    when isDirectory $
        mapDirectory fpFrom $ \f -> putFile aws (Local f) r

    -- directory does not exist
    mapDirectory fpFrom $ \f -> do
        let dst = fpTo </> stripLeadingSlash (stripPrefix fpFrom f)
        putFile aws (Local f) (Remote b dst)

-- | Test if the remote argument is what we would consider a directory. 
--   Beware sketchy algorithm ahead...
remoteIsDirectory :: AWSConnection -> Remote -> IO Bool
remoteIsDirectory aws (Remote b fp) = do
    let req = ListRequest (addTrailingSlash fp) "" "" 1
    resp <- listObjects aws b req
    case resp of
        Left  _       -> return False
        Right (_, []) -> return False
        Right (_, _ ) -> return True

-- | List the contents of a remote directory
remoteListDirectory :: AWSConnection -> Remote -> IO [Remote]
remoteListDirectory aws remote@(Remote b _) = do
    results <- listDirectory "" aws remote
    return $ map (Remote b . key) results

-- | Factored out for reuse
listDirectory :: String -> AWSConnection -> Remote -> IO [ListResult]
listDirectory m aws remote@(Remote b fp) = do
    let req = ListRequest (addTrailingSlash fp) m "" 1000
    resp <- listObjects aws b req
    case resp of
        Left e -> do
            hPutStrLn stderr $ show e
            return []

        -- TODO: empty or directory not found?
        Right (_,     []     ) -> return []
        Right (trunc, thisSet) ->
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
        let p = dir </> name
        isDirectory <- doesDirectoryExist p
        if isDirectory then mapDirectory p f else f p

-- | Presense of a colon means remote arg, break it; else, it's a local 
--   filepath. TODO: be smarter about that.
parseArg :: String -> Arg
parseArg arg = let b = takeWhile (/= ':') arg
                   len = length b
               in if len == length arg
                    then L $ Local arg -- local filepath
                    else R . Remote b . stripLeadingSlash $ drop (len+1) arg

-- | Validate that a list of arguments are all local or all remote
allSame :: [Arg] -> Bool
allSame []  = True
allSame [_] = True
allSame (a:b:rest) = a `sameAs` b && allSame (b:rest)

sameAs :: Arg -> Arg -> Bool
sameAs (L _) (L _) = True
sameAs (R _) (R _) = True
sameAs _     _     = False

allLocal :: [Arg] -> Bool
allLocal = all local
    where
        local (L _) = True
        local _     = False

allRemote :: [Arg] -> Bool
allRemote = all remote
    where
        remote (R _) = True
        remote _     = False

baseName :: FilePath -> FilePath
baseName = snd . splitFileName

stripPrefix :: String -> String -> String 
stripPrefix pref str =
    if pref `isPrefixOf` str
        then drop (length pref) str
        else str

addTrailingSlash :: FilePath -> FilePath
addTrailingSlash = reverse . addSlash . reverse
    where
        addSlash :: FilePath -> FilePath
        addSlash s@('/':_) = s
        addSlash s         = '/':s

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
