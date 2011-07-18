module Main where

import Network.AWS.AWSConnection
import Network.AWS.Utils

import Control.Monad (forM_, guard)
import System.IO     (hPutStrLn, stderr)

main :: IO ()
main = handleArgs usage parseArgs $ \(srcs, dst) ->
    forM_ srcs $ \src -> copy src dst

usage :: IO ()
usage = putStrLn $ unlines
    [ "usage: s3cp <path> ... bucket:[<path>]"
    , "       s3cp <bucket:path> ... <path>  "
    ]

parseArgs :: [String] -> Maybe ([Arg], Arg)
parseArgs []   = Nothing
parseArgs [_]  = Nothing
parseArgs args = do
    let srcs = map parseArg $ init args
    let dst  =     parseArg $ last args

    -- all sources must be the same type
    guard (allSame srcs)

    return (srcs,dst)

copy :: Arg -> Arg -> IO ()
copy (R remote) (L local) = do
    mconn <- amazonS3ConnectionFromEnv
    case mconn of
        Just conn -> pullObject conn remote local
        _         -> hPutStrLn stderr errorEnvNotSet

copy (L local) (R remote) = do
    mconn <- amazonS3ConnectionFromEnv
    case mconn of
        Just conn -> pushObject conn local remote
        _         -> hPutStrLn stderr errorEnvNotSet

copy (R from) (R to) = do
    mconn <- amazonS3ConnectionFromEnv
    case mconn of
        Just conn -> copyRemote conn from to
        _         -> hPutStrLn stderr errorEnvNotSet
        
copy _ _ = hPutStrLn stderr errorInvalidArgs
