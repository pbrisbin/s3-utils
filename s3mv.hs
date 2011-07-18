module Main where

import Network.AWS.Utils
import Control.Monad (forM_, guard)

main :: IO ()
main = handleArgs usage parseArgs $ \(srcs, dst) ->
    forM_ srcs $ \src -> move src dst

usage :: String
usage = "s3mv <bucket:[path]> <bucket:[path]>"

parseArgs :: [String] -> Maybe ([Arg], Arg)
parseArgs []   = Nothing
parseArgs [_]  = Nothing
parseArgs args = do
    let srcs = map parseArg $ init args
    let dst  =     parseArg $ last args

    -- all sources must be remote
    guard (allRemote srcs)

    return (srcs,dst)

move :: Arg -> Arg -> IO ()
move (R from) (R to) = withConnection $ \aws -> moveRemote aws from to
move _        _      = errorInvalidArgs
