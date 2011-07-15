-- TODO: limit exports
module Network.AWS.Utils
    ( Bucket
    , Local(..)
    , Remote(..)
    , putFile
    , getFile
    , mapDirectory
    , walkDirectory
    , errorInvalidArgs
    , errorEnvNotSet
    ) where

import Network.AWS.AWSConnection
import Network.AWS.AWSResult
import Network.AWS.S3Object

import Control.Exception  (IOException, handle)
import Control.Monad      (forM)
import System.Directory   (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath    ((</>))
import System.IO          (hPutStrLn, stderr)
import Network.Wai.Application.Static (defaultMimeTypeByExt)

import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8      as C8

type Bucket = String

-- | A file or directory on the local system
data Local = Local
    { filePath :: FilePath
    }

-- | A file "in the cloud"
data Remote = Remote
    { bucket :: Bucket
    , path   :: FilePath
    }

-- | Uploads the local file to the remote location, prints any errors to 
--   stdout
putFile :: AWSConnection -> Local -> Remote -> IO ()
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
    -- a skeleton object, identifies the file by bucket/path
    let obj = S3Object b fpFrom "" [] (L8.pack "")
    resp <- getObject aws obj
    case resp of
        Left e -> hPutStrLn stderr $ show e
        Right ob -> do
            B.writeFile fpTo (obj_data obj)
            -- todo: -v option

strip :: FilePath -> FilePath
strip ('/':rest)         = strip rest
strip ('.':'/':rest)     = strip rest
strip ('.':'.':'/':rest) = strip rest
strip x                  = x

skipFile :: IOException -> IO ()
skipFile e = hPutStrLn stderr $ show e

mapDirectory :: FilePath -> (FilePath -> IO ()) -> IO ()
mapDirectory fp f = do
    paths <- walkDirectory fp
    mapM_ f paths

walkDirectory :: FilePath -> IO [FilePath]
walkDirectory dir = handle skipDir $ do
    names <- getDirectoryContents dir
    let pnames = filter (`notElem` [".", ".."]) names
    paths <- forM pnames $ \name -> do
        let path = dir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then walkDirectory path
            else return [path]

    return $ concat paths

    where
        -- skip unreadable dirs
        skipDir :: IOException -> IO [FilePath]
        skipDir e = do
            hPutStrLn stderr $ show e
            return []

errorInvalidArgs :: ReqError
errorInvalidArgs = AWSError "" "Invalid arguments for operation"

errorEnvNotSet :: ReqError
errorEnvNotSet= AWSError "" "AWS environment variables are not set"
