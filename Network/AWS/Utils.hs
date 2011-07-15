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

    -- * Helpers
    , mapDirectory
    , parseArg
    , allSameType

    -- * Error messages
    , errorInvalidArgs
    , errorEnvNotSet
    ) where

import Network.AWS.AWSConnection
import Network.AWS.AWSResult
import Network.AWS.S3Object
import Network.Wai.Application.Static (defaultMimeTypeByExt)

import Control.Exception  (IOException, handle)
import Control.Monad      (forM_)
import System.Directory   (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.FilePath    (splitFileName, (</>))
import System.IO          (hPutStrLn, stderr)

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

putFile aws (Local fpFrom) (Remote b fpTo) = handle skipFile $ do
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
getFile aws (Remote b fpFrom) (Local fpTo) = handle skipFile $ do
    -- if fpTo is a directory, put basename of the the from file into 
    -- that directory
    isDirectory <- doesDirectoryExist fpTo
    let fpTo' = if isDirectory then baseName fpFrom else fpTo

    -- a skeleton object, identifies the file by bucket/path
    let obj = S3Object b fpFrom "" [] (L8.pack "")
    resp <- getObject aws obj
    case resp of
        Left e     -> hPutStrLn stderr $ show e
        Right obj' -> B.writeFile fpTo' (obj_data obj')

skipFile :: IOException -> IO ()
skipFile e = hPutStrLn stderr $ show e

baseName :: FilePath -> FilePath
baseName = snd . splitFileName

-- | Recursively execute a function on all files in the passed directory
mapDirectory :: FilePath -> (FilePath -> IO ()) -> IO ()
mapDirectory dir f = do
    names <- getDirectoryContents dir
    let pnames = filter (`notElem` [".", ".."]) names
    forM_ pnames $ \name -> do
        let path = dir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory then mapDirectory path f else f path

    where
        -- skip unreadable dirs
        skipDir :: IOException -> IO [FilePath]
        skipDir e = do
            hPutStrLn stderr $ show e
            return []

-- | Presense of a colon means remote arg, break it; else, it's a local 
--   filepath. TODO: be smarter about that.
parseArg :: String -> Arg
parseArg arg = let bucket = takeWhile (/= ':') arg
                   len = length bucket
               in if len == length arg
                    then L $ Local arg -- local filepath
                    else R . Remote bucket . fix $ drop (len+1) arg

    where
        fix :: String -> String
        fix ('/':rest) = fix rest
        fix x          = x

-- | Validate that a list of arguments are all local or all remote
allSameType :: [Arg] -> Bool
allSameType []  = True
allSameType [_] = True
allSameType (a:b:rest) = a `sameAs` b && allSameType (b:rest)

sameAs :: Arg -> Arg -> Bool
sameAs (L _) (L _) = True
sameAs (R _) (R _) = True
sameAs _     _     = False

-- | Invalid arguments for operation
errorInvalidArgs :: String
errorInvalidArgs = "Invalid arguments for operation"

-- | AWS environment variables are not set
errorEnvNotSet :: String
errorEnvNotSet = "AWS environment variables are not set"
