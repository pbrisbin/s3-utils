module Main where

import Network.AWS.Utils

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

data CopyArg = L Local | R Remote

main :: IO ()
main = undefined

copy :: CopyArg -> CopyArg -> IO (AWSResult ())

-- Copy the bucket files to the local destination
--
-- todo: if bucket dest is a directory, copy it and its contents
--
copy (R remote) (L local) = do
    mconn <- amazonS3ConnectionFromEnv
    case mconn of
        Just conn -> do
            getFile conn remote local
            return $ Right ()
        _         -> return $ Left errorEnvNotSet

-- Copy the local file(s) up to the bucket
--
-- todo: s3cp foo.txt bucket:foo.txt should create bucket/foo.txt
--       s3cp foo.txt bucket:bar     should create bucket/bar/foo.txt
--
--       but there's no way to tell the user's intention
--
copy (L local) (R remote) = do
    mconn <- amazonS3ConnectionFromEnv
    case mconn of
        Just conn -> do
            isDirectory <- doesDirectoryExist $ filePath local
            if isDirectory
                then mapDirectory (filePath local ) $ \fp -> do
                    copy (L $ Local fp) (R remote)
                    return ()
                else putFile conn local remote
            return $ Right ()
        _ -> return $ Left errorEnvNotSet

-- TODO: support the other cases
copy _ _ = return $ Left errorInvalidArgs
