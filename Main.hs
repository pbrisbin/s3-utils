-- https://github.com/pbrisbin/s3
module Main where

import Config

import Network.AWS.S3Object

import Control.Exception  (IOException, handle)
import Control.Monad      (forM)
import System.Directory   (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath    ((</>))
import System.IO          (hPutStrLn, stderr)
import Network.Wai.Application.Static (defaultMimeTypeByExt)

import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8      as C8

main :: IO ()
main = handleArgs =<< getArgs

handleArgs :: [String] -> IO ()
handleArgs []          = putStrLn "usage: s3 [ --get ] <path> ..."
handleArgs args@(a:as) =
    if a == "--get"
        then mapM_ getArg  as
        else mapM_ sendArg args

sendArg :: FilePath -> IO ()
sendArg fp = do
    isDirectory <- doesDirectoryExist fp
    if isDirectory
        then mapM_ sendFile =<< walkDirectory fp
        else do
            isFile <- doesFileExist fp
            if isFile
                then sendFile fp
                else return ()

-- AWS will 404 on directory argument
getArg :: FilePath -> IO ()
getArg = getFile

sendFile :: FilePath -> IO ()
sendFile fp = handle skipFile $ do
    obj <- do
        fileData <- B.readFile fp
        return S3Object
            { obj_bucket   = myBucket
            , obj_name     = strip fp
            , content_type = C8.unpack $ defaultMimeTypeByExt fp
            , obj_headers  = []
            , obj_data     = fileData
            }

    resp <- sendObject myConnection obj

    case resp of
        Left  e -> hPutStrLn stderr $ show e
        Right _ -> putStrLn $ "Sent " ++ fp

getFile :: FilePath -> IO ()
getFile fp = handle skipFile $ do
    resp <- getObject myConnection $ skel fp
    case resp of
        Left  e   -> hPutStrLn stderr $ show e
        Right obj -> do
            B.writeFile fp (obj_data obj)
            putStrLn $ "Created " ++ fp

    where
        skel :: FilePath -> S3Object
        skel f = S3Object myBucket (strip f) "" [] (L8.pack "")

strip :: FilePath -> FilePath
strip ('/':rest)         = strip rest
strip ('.':'/':rest)     = strip rest
strip ('.':'.':'/':rest) = strip rest
strip x                  = x

-- skip unreadable files
skipFile :: IOException -> IO ()
skipFile e = hPutStrLn stderr $ show e

walkDirectory :: FilePath -> IO [FilePath]
walkDirectory dir = handle skipDir $ do
    names <- getDirectoryContents dir
    let pnames = filter (`notElem` [".", ".."]) names
    paths <- forM pnames $ \name -> do
        let path = dir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then walkDirectory path
            else return [path]

    return $ concat paths

    where
        -- skip unreadable dirs
        skipDir :: IOException -> IO [FilePath]
        skipDir e = do
            hPutStrLn stderr $ show e
            return []
