module Main where

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

data Source = SBucket Bucket FilePath
            | SLocal [FilePath]

data Destination = DBucket Bucket FilePath
                 | DLocal FilePath

main :: IO ()
main = undefined

s3cp :: Source -> Destination -> IO (AWSResult ())

-- Copy the bucket files to the local destination
--
-- todo: if bucket dest is a directory, copy it and it's contents
--
s3cp (SBucket b bfp) (DLocal fp) = do
    mconn <- amazonS3ConnectionFromEnv
    case mconn of
        Just conn -> do
            getFile conn b (strip bfp) fp
            return $ Right ()
        _         -> return $ Left awsEnvNotSet

-- Copy the local file(s) up to the bucket
--
-- todo: s3cp foo.txt bucket:foo.txt should create bucket/foo.txt
--       s3cp foo.txt bucket:bar     should create bucket/bar/foo.txt
--
--       but there's no way to tell the user's intention
--
s3cp (SLocal fps) (DBucket b bfp) = do
    mconn <- amazonS3ConnectionFromEnv
    case mconn of
        Just conn -> do
            _ <- forM fps $ \fp -> do
                let ffp = strip bfp </> fp
                putFile conn b fp ffp
            return $ Right ()
        _ -> return $ Left awsEnvNotSet

-- invalid usage, throw error
s3cp _ _ = return $ Left awsInvalidArgs

putFile :: AWSConnection -> Bucket -> FilePath -> FilePath -> IO ()
putFile aws b fpFrom fpTo = handle skipFile $ do
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
        Right _ -> return () -- todo: -v option
    
getFile :: AWSConnection -> Bucket -> FilePath -> FilePath -> IO ()
getFile aws b fpFrom fpTo = handle skipFile $ do
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

-- custom errors
awsInvalidArgs :: ReqError
awsInvalidArgs = AWSError "" "Arguments for copy are invalid"

awsEnvNotSet :: ReqError
awsEnvNotSet= AWSError "" "AWS environment variables are not set"
