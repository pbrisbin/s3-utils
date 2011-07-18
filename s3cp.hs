module Main where

import Network.AWS.Utils
import Control.Monad (forM_, guard)

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
copy (R remote) (L local ) = withConnection $ \aws -> pullObject aws remote local
copy (L local ) (R remote) = withConnection $ \aws -> pushObject conn local remote
copy (R from  ) (R to    ) = withConnection $ \aws -> copyRemote conn from to
copy _          _          = errorInvalidArgs
