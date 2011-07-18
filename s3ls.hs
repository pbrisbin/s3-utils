module Main where

import Network.AWS.S3Bucket
import Network.AWS.Utils
import Control.Monad (guard)

main :: IO ()
main = handleArgs usage parseArgs $ mapM_ ls

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
ls remote@(Remote _ fp) = withConnection $ \aws -> do
    isDirectory <- remoteIsDirectory aws remote
    results <- if null fp || isDirectory
        then listDirectory "" aws remote
        else do
            resp <- listDirectory "" aws remote
            return $ filter ((== fp) . key) resp

    mapM_ printResult results

printResult :: ListResult -> IO ()
printResult (ListResult k m e s _) = putStrLn $ unwords [ m, e, prettySize s, k ]

-- print raw bytes in human readable
prettySize :: Integer -> String
prettySize s
    | s >= 1000          && s < 1000000       = printFloat (fromIntegral s / 1024) "KB"
    | s >= 1000000       && s < 1000000000    = printFloat (fromIntegral s / 1024 * 1024) "MB"
    | s >= 1000000000    && s < 1000000000000 = printFloat (fromIntegral s / 1024 * 1024 * 1024) "GB"
    | s >= 1000000000000                      = printFloat (fromIntegral s / 1024 * 1024 * 1024 * 1024) "TB"
    | otherwise                               = printFloat (fromIntegral s) "B"

-- print a float to two decimals
printFloat :: Double -> String -> String
printFloat n s = let (w,d) = properFraction n :: (Integer, Double)
    in pad 10 $ show w ++ "." ++ (take 2 . drop 1 . dropWhile (/= '.') $ show d) ++ s

-- pads or trims a value to fit a certain number of characters
pad :: Int -> String -> String
pad lim str
    | length str > lim = take (lim - 3) str ++ "..."
    | otherwise        = let len = length str in
        if length str < lim
            then replicate (lim - len) ' ' ++ str
            else str
