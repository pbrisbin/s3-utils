module Main where

import Network.AWS.AWSConnection
import Network.AWS.Utils

import Control.Monad (guard)
import System.IO     (hPutStrLn, stderr)

main :: IO ()
main = handleArgs usage parseArgs $ mapM_ rm

usage :: IO ()
usage = putStrLn "usage: s3rm <bucket:[path]> ..."

-- rm for remote dirs only
parseArgs :: [String] -> Maybe [Remote]
parseArgs []   = Nothing
parseArgs args = do
    let dirs = map parseArg args
    guard (allRemote dirs)
    return $ map unRemote dirs

    where
        -- undefined is safe due to guard above
        unRemote :: Arg -> Remote
        unRemote (R remote) = remote
        unRemote _          = undefined

rm :: Remote -> IO ()
rm remote = do
    mconn <- amazonS3ConnectionFromEnv
    case mconn of
        Just conn -> removeRemote conn remote
        _         -> hPutStrLn stderr errorEnvNotSet
