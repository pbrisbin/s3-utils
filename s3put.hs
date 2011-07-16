module Main where

import Network.AWS.AWSConnection
import Network.AWS.S3Object
import Network.AWS.Utils

import Control.Monad      (guard)
import System.Environment (getArgs)
import System.IO          (hPutStrLn, stderr)

import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Just r -> put r
        _      -> usage

usage :: IO ()
usage = putStrLn "usage: command | s3put <bucket:path>"

parseArgs :: [String] -> Maybe Remote
parseArgs [arg] = do
    let rarg = parseArg arg
    guard (allRemote [rarg])

    let r = unRemote rarg
    guard (path r /= "")

    return r

    where
        -- undefined is safe due to guard above
        unRemote :: Arg -> Remote
        unRemote (R remote) = remote
        unRemote _          = undefined

-- must be a singleton list
parseArgs _ = Nothing

put :: Remote -> IO ()
put remote@(Remote b fp) = do
    mconn <- amazonS3ConnectionFromEnv
    case mconn of
        Just conn -> do
            obj <- do
                -- read from stdin
                fileData <- B.getContents
                return S3Object
                    { obj_bucket   = b
                    , obj_name     = fp
                    , content_type = ""
                    , obj_headers  = []
                    , obj_data     = fileData
                    }

            resp <- sendObject conn obj
            case resp of
                Left e  -> hPutStrLn stderr $ show e
                Right _ -> putStrLn $ "written: " ++ b ++ ":" ++ fp

        _ -> hPutStrLn stderr errorEnvNotSet
