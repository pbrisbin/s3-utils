module Main where

import Network.AWS.S3Bucket
import Network.AWS.Utils
import Control.Monad (guard)

main :: IO ()
main = handleArgs usage parseArgs $ mapM_ ls

usage :: String
usage = "usage: s3ls <bucket:[path]> ..."

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
ls remote@(Remote _ fp) = withConnection $ \aws -> do
    isDirectory <- remoteIsDirectory aws remote

    let filt = if null fp || isDirectory
        then id else filter ((== fp) . key)

    resp <- listDirectory "" aws remote
    mapM_ prettyResult $ filt resp

prettyResult :: ListResult -> IO ()
prettyResult (ListResult k m e s _) = putStrLn $ unwords [ m, e, prettySize s, k ]

    where
        prettySize :: Integer -> String
        prettySize b
            | b >= 1000          && b < 1000000       = pad 12 $ printFloat (fromIntegral b / 1024) ++ "KB"
            | b >= 1000000       && b < 1000000000    = pad 12 $ printFloat (fromIntegral b / 1024 * 1024) ++ "MB"
            | b >= 1000000000    && b < 1000000000000 = pad 12 $ printFloat (fromIntegral b / 1024 * 1024 * 1024) ++ "GB"
            | b >= 1000000000000                      = pad 12 $ printFloat (fromIntegral b / 1024 * 1024 * 1024 * 1024) ++ "TB"
            | otherwise                               = pad 12 $ printFloat (fromIntegral b) ++ "B"

        printFloat :: Double -> String
        printFloat n = let (w,d) = properFraction n :: (Integer, Double)
            in show w ++ "." ++ (take 2 . drop 1 . dropWhile (/= '.') $ show d)

        pad :: Int -> String -> String
        pad lim str
            | length str > lim = take (lim - 3) str ++ "..."
            | length str < lim = replicate (lim - length str) ' ' ++ str
            | otherwise        = str
