module Main where

import Network.AWS.AWSConnection
import Network.AWS.Utils

import Control.Monad      (forM_)
import System.Directory   (doesDirectoryExist)
import System.Environment (getArgs)
import System.IO          (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Just (srcs,dst) -> forM_ srcs $ \src -> copy src dst
        _               -> usage

-- TODO: help message
usage :: IO ()
usage = putStrLn $ unlines
    [ "usage: s3cp <path> ... bucket:[<path>]"
    , "       s3cp <bucket:path> ... <path>  "
    ]

parseArgs :: [String] -> Maybe ([Arg], Arg)
parseArgs []   = Nothing
parseArgs [_]  = Nothing
parseArgs args =
    let cargs@(srcs,dst) = (,) (map parseArg $ init args) (parseArg $ last args)
    in if allSameType srcs
        then Just cargs
        else Nothing

-- | The copy operation
copy :: Arg -> Arg -> IO ()

-- Copy the bucket file to the local destination
copy (R remote) (L local) = do
    mconn <- amazonS3ConnectionFromEnv
    case mconn of
        Just conn -> do
            isDirectory <- remoteIsDirectory conn remote
            if isDirectory
                then getDirectory conn remote local
                else getFile conn remote local
        _         -> hPutStrLn stderr errorEnvNotSet

-- Copy the local file(s) up to the bucket
copy (L local) (R remote) = do
    mconn <- amazonS3ConnectionFromEnv
    case mconn of
        Just conn -> do
            isDirectory <- doesDirectoryExist $ filePath local
            if isDirectory
                then mapDirectory (filePath local) $ \fp -> putFile conn (Local fp) remote
                else putFile conn local remote
        _ -> hPutStrLn stderr errorEnvNotSet

-- TODO: support the other cases?
copy _ _ = hPutStrLn stderr errorInvalidArgs
