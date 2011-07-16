module Main where

import Network.AWS.AWSConnection
import Network.AWS.S3Bucket
import Network.AWS.S3Object
import Network.AWS.Utils

import Control.Monad (guard)
import System.IO     (hPutStrLn, stderr)

import qualified Data.ByteString.Lazy.Char8 as L8

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
rm remote@(Remote b fp) = do
    mconn <- amazonS3ConnectionFromEnv
    case mconn of
        Just conn -> do
            if null fp

                then do -- remove the whole bucket
                    resp <- emptyBucket conn b
                    case resp of
                        Left e  -> hPutStrLn stderr $ show e
                        Right _ -> do
                            resp' <- deleteBucket conn b
                            case resp' of
                                Left e  -> hPutStrLn stderr $ show e
                                Right _ -> putStrLn $ "removed: " ++ b ++ ":"

                else do -- remove the file/dir
                    isDirectory <- remoteIsDirectory conn remote
                    if isDirectory
                        then do
                            remotes <- remoteListDirectory conn remote
                            mapM_ rm remotes
                        else do
                            resp'' <- deleteObject conn $ S3Object b fp "" [] (L8.pack "")
                            case resp'' of
                                Left e  -> hPutStrLn stderr $ show e
                                Right _ -> putStrLn $ "removed: " ++ b ++ ":" ++ fp

        _ -> hPutStrLn stderr errorEnvNotSet
