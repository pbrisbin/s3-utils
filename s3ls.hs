module Main where

import Network.AWS.AWSConnection
import Network.AWS.S3Bucket
import Network.AWS.Utils

import Control.Monad      (guard)
import System.Environment (getArgs)
import System.IO          (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Just remotes -> mapM_ ls remotes
        _            -> usage

usage :: IO ()
usage = putStrLn "usage: s3ls <bucket:[path]> ..."

-- ls for remote dirs only
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

ls :: Remote -> IO ()
ls remote@(Remote _ fp) = do
    mconn <- amazonS3ConnectionFromEnv
    case mconn of
        Just conn -> do
            isDirectory <- remoteIsDirectory conn remote
            results <- if null fp || isDirectory
                then listDirectory "" conn remote
                else do
                    resp <- listDirectory "" conn remote
                    return $ filter ((== fp) . key) resp

            mapM_ printResult results

        _ -> hPutStrLn stderr errorEnvNotSet

printResult :: ListResult -> IO ()
printResult (ListResult k m e s _) = putStrLn $ unwords [ m , e , pad 10 $ show s , k ]
    where
        -- note, truncates values
        pad :: Int -> String -> String
        pad lim str
            | length str > lim = take (lim - 3) str ++ "..."
            | otherwise        = let len = length str in
                if length str < lim
                    then str ++ replicate (lim - len) ' '
                    else str
