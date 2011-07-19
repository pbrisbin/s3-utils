-------------------------------------------------------------------------------
-- |
-- Module      :  Network.AWS.Utils
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
-- Maintainer  :  pbrisbin@gmail.com 
-- Stability   :  unstable
-- Portability :  unportable
--
-- Convenience wrappers around the S3 library provided by hS3. 
--
-- Intelligently handles the difference between buckets, directories and 
-- files as arguments to push, pull, copy, move, and remove operations. 
-- Behavior meant to mimic core utils
--
-------------------------------------------------------------------------------
module Network.AWS.Utils
    ( Bucket
    , Local(..)
    , Remote(..)
    , withConnection

    -- * Local-Remote actions
    , pushObject
    , pullObject

    -- * Remote actions
    , copyRemote
    , moveRemote
    , removeRemote

    -- * List actions
    , remoteIsDirectory
    , remoteListDirectory
    , listDirectory

    -- * Argument processing
    , Arg(..)
    , handleArgs
    , parseArg
    , allSame
    , allLocal
    , allRemote

    -- * Errors
    , handleError
    , errorInvalidArgs
    , errorEnvNotSet
    ) where

import Network.AWS.AWSConnection
import Network.AWS.AWSResult
import Network.AWS.S3Object
import Network.AWS.S3Bucket

import Control.Exception  (IOException, handle)
import Control.Monad      (forM, forM_)
import Data.Char          (toLower)
import Data.List          (isPrefixOf)
import System.Environment (getArgs)
import System.FilePath    (splitFileName, takeExtension, (</>))
import System.IO          (hPutStrLn, stderr)

import System.Directory

import qualified Data.Map                   as M
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as L8

type Bucket = String

data Local = Local { filePath :: FilePath }

data Remote = Remote
    { bucket :: Bucket
    , path   :: FilePath
    }

data Arg = L Local | R Remote

-- | Upload a local file or directory
--
--   If the source is a file, the basename is copied to the destination. 
--
--   When the destination is blank, it is copied to the top level of the 
--   bucket. If the destination exists and is a file it is overwritten. 
--   When the destination exists and is a directory the file is copied 
--   into it. When the destination does not exist, the file is created 
--   there.
--
--   If the source is a directory, its contents are copied recursively.
--
--   When the destination is blank, it is copied to the top level of the 
--   bucket. If the destination exists and is a directory, the source 
--   directory is copied into it. If the destination does not exist, it 
--   is created as a directory and the source directory's contents are 
--   copied into it. If the destination exists and is a file... I'm not 
--   sure what will happen. S3 may allow files and directories of the 
--   same name since the actual object key is the full path to any 
--   contents; directories don't really exist.
--
pushObject :: AWSConnection -> Local -> Remote -> IO ()
pushObject aws local@(Local fp) remote = do
    isDirectory <- doesDirectoryExist fp
    if isDirectory
        then pushFile      local remote
        else pushDirectory local remote

    where
        pushFile :: Local -> Remote -> IO ()
        pushFile l@(Local fpFrom)   (Remote b ""  ) = pushFile l $ Remote b $ baseName fpFrom
        pushFile   (Local fpFrom) r@(Remote b fpTo) =
            handle skip $ do
                isDirectory <- remoteIsDirectory aws r

                let f = if isDirectory
                            then if fpFrom == "-"
                                then fpTo </> "stdin.txt"
                                else fpTo </> baseName fpFrom
                            else fpTo

                let c = if fpFrom == "-"
                            then getMimeType f
                            else getMimeType fpFrom

                fileData <- if fpFrom == "-"
                    then B.getContents -- stdin
                    else B.readFile fpFrom

                resp <- sendObject aws S3Object
                    { obj_bucket   = b
                    , obj_name     = f
                    , content_type = c
                    , obj_headers  = []
                    , obj_data     = fileData
                    }

                handleError resp $ \_ -> putStrLn $ fpFrom ++ " -> " ++ b ++ ":" ++ f

        pushDirectory :: Local -> Remote -> IO ()
        pushDirectory l@(Local fpFrom)   (Remote b ""  ) = pushDirectory l $ Remote b $ baseName fpFrom
        pushDirectory   (Local fpFrom) r@(Remote b fpTo) = do
            isDirectory <- remoteIsDirectory aws r
            if isDirectory
                then mapDirectory fpFrom $ \f -> pushFile (Local f) r
                else mapDirectory fpFrom $ \f -> do
                    -- merge the full paths so the final pathnames make 
                    -- sense for what we're tryign to do
                    let dst = fpTo </> stripLeadingSlash (stripPrefix fpFrom f)
                    pushFile (Local f) (Remote b dst)

-- | Download a file or directory from S3
--
--   Same rules apply as with @'pushObject'@ regarding directory 
--   handling except that destination cannot be blank in this case.
--
--   If the destination exists and is a file, you should receive an 
--   IOException. Linux will not allow creation of directories with the 
--   same name as existing files (and vice versa).
--
pullObject :: AWSConnection -> Remote -> Local -> IO ()
pullObject aws remote local = do
    isDirectory <- remoteIsDirectory aws remote
    if isDirectory
        then pullDirectory remote local
        else pullFile      remote local

    where
        pullFile :: Remote -> Local -> IO ()
        pullFile (Remote b fpFrom) (Local fpTo) =
            handle skip $ do
                isDirectory <- doesDirectoryExist fpTo

                let f = if isDirectory
                            then fpTo </> baseName fpFrom
                            else fpTo

                resp <- getObject aws $ S3Object b fpFrom "" [] (L8.pack "")

                handleError resp $ \o -> do
                    if fpTo == "-"
                        then B.putStr (obj_data o)
                        else do
                            B.writeFile f (obj_data o)
                            putStrLn $ b ++ ":" ++ fpFrom ++ " -> " ++ f

        pullDirectory :: Remote -> Local -> IO ()
        pullDirectory r@(Remote _ fpFrom) (Local fpTo) =
            handle skip $ do
                isDirectory <- doesDirectoryExist fpTo
                if isDirectory
                    then do -- directory exists
                        remotes <- remoteListDirectory aws r
                        forM_ remotes $ \r' ->
                            go remote $ fpTo </> path r'

                    else do -- directory does not exist
                        remotes <- remoteListDirectory aws r
                        forM_ remotes $ \_ ->
                            go remote $ fpTo </> stripLeadingSlash (stripPrefix fpFrom fpTo)

        -- common logic
        go ::  Remote -> FilePath -> IO ()
        go remote' dst' = do
            let (dir,_) = splitFileName dst'
            createDirectoryIfMissing True dir
            pullFile remote' (Local dst')

-- | Copy a remote directory or file
--
--   Same rules apply as with @'pushObject'@ and @'pullObject'@ 
--   regarding directory handling.
--
--   At this point, copying an entire bucket is not supported.
--
copyRemote :: AWSConnection -> Remote -> Remote -> IO ()
copyRemote aws from to = handle skip $ do
    fromTos <- do
        isDirectory <- remoteIsDirectory aws from
        if isDirectory
            then setupDirCopy  from to
            else setupFileCopy from to

    forM_ fromTos $ \(src, dst) -> do
        resp <- copyObjectWithReplace aws src dst

        handleError resp $ \_ -> putStrLn $
            (obj_bucket src) ++ ":" ++ (obj_name src) ++ "->" ++
            (obj_bucket dst) ++ ":" ++ (obj_name dst)

    where
        -- setup a list of source and destination objects when the 
        -- source for the copy is a directory
        setupDirCopy :: Remote -> Remote -> IO [(S3Object, S3Object)]
        setupDirCopy f@(Remote bFrom fpFrom) t@(Remote _ fpTo) = do
            isDirectory <- remoteIsDirectory   aws t
            remotes     <- remoteListDirectory aws f

            forM remotes $ \_ -> do
                fpTo' <- if isDirectory
                    then return $ fpTo </> fpFrom
                    else return $ fpTo </> stripLeadingSlash (stripPrefix fpFrom fpTo)

                let srcObj = S3Object bFrom fpFrom "" [] (L8.pack "")
                let dstObj = S3Object bFrom fpTo'  "" [] (L8.pack "")

                return (srcObj, dstObj)
             
        -- setup a list of source and desitination objects when the 
        -- source for the copy is a file
        setupFileCopy :: Remote -> Remote -> IO [(S3Object, S3Object)]
        setupFileCopy (Remote bFrom fpFrom) r@(Remote _ fpTo) = do
            fpTo' <- if fpTo == ""
                then return $ baseName fpFrom
                else do
                    isDirectory <- remoteIsDirectory aws r
                    if isDirectory
                        then return $ fpTo </> baseName fpFrom
                        else return fpTo

            let srcObj = S3Object bFrom fpFrom "" [] (L8.pack "")
            let dstObj = S3Object bFrom fpTo'  "" [] (L8.pack "")

            return [(srcObj, dstObj)]

-- | Move a remote directory or file
--
--   This is just copy-then-remove.
--
moveRemote :: AWSConnection -> Remote -> Remote -> IO ()
moveRemote aws rFrom rTo = copyRemote aws rFrom rTo >> removeRemote aws rFrom

-- | Remove a remote bucket, directory or file
--   
--   If the argument is a bucket, it is emptied and then deleted. If the 
--   argument is a directory it is removed recursively.
--
removeRemote :: AWSConnection -> Remote -> IO ()
removeRemote aws (Remote b "") = handle skip $ do
    resp <- emptyBucket aws b
    handleError resp $ \_ -> do
        resp' <- deleteBucket aws b
        handleError resp' $ \_ -> putStrLn $ "removed: " ++ b ++ ":"

removeRemote aws remote@(Remote b fp) = handle skip $ do
    isDirectory <- remoteIsDirectory aws remote
    if isDirectory
        then do -- remove entire directory
            remotes <- remoteListDirectory aws remote
            mapM_ (removeRemote aws) remotes

        else do -- remove file
            resp'' <- deleteObject aws $ S3Object b fp "" [] (L8.pack "")
            handleError resp'' $ \_ -> putStrLn $ "removed: " ++ b ++ ":" ++ fp

-- | Test if the remote argument is what we would consider a directory
--
--   This is accomplished by looking for at least one object with the 
--   directory name (and trailing slash) as a prefix. This requires a 
--   LIST command.
--
remoteIsDirectory :: AWSConnection -> Remote -> IO Bool
remoteIsDirectory aws (Remote b fp) = do
    let req = ListRequest (addTrailingSlash fp) "" "" 1
    resp <- listObjects aws b req
    case resp of
        Left  _       -> return False
        Right (_, []) -> return False
        Right (_, _ ) -> return True

-- | List the contents of a remote directory as @'Remote'@s
remoteListDirectory :: AWSConnection -> Remote -> IO [Remote]
remoteListDirectory aws remote@(Remote b _) = do
    results <- listDirectory "" aws remote
    return $ map (Remote b . key) results

-- | A raw LIST action
--
--   This is used by @'remoteListDirectory'@. It was factored out for 
--   access to the raw @'ListResult'@s.
--
listDirectory :: String -> AWSConnection -> Remote -> IO [ListResult]
listDirectory m aws remote@(Remote b fp) = do
    let fp' = if null fp then fp else addTrailingSlash fp
    let req = ListRequest fp' m "" 1000
    resp <- listObjects aws b req
    case resp of
        Left e -> do
            hPutStrLn stderr $ show e
            return []

        -- TODO: empty or directory not found?
        Right (_,     []     ) -> return []
        Right (trunc, thisSet) ->
            if trunc
                then do
                    let lastItem = key $ last thisSet
                    nextSet <- listDirectory lastItem aws remote
                    return $ init thisSet ++ nextSet
                else return thisSet

-- | Recursively execute a function on all files in a local directory
mapDirectory :: FilePath -> (FilePath -> IO ()) -> IO ()
mapDirectory dir f = handle skip $ do
    names <- getDirectoryContents dir
    let pnames = filter (`notElem` [".", ".."]) names
    forM_ pnames $ \name -> do
        let p = dir </> name
        isDirectory <- doesDirectoryExist p
        if isDirectory then mapDirectory p f else f p

-- | Parse a string argument into either a local or remote filepath by 
--   looking for the presense of a colon (ie. bucket:path).
parseArg :: String -> Arg
parseArg arg =
    let b   = takeWhile (/= ':') arg
        len = length b
    in if len == length arg
        then L $ Local arg -- local filepath
        else R . Remote b . stripLeadingSlash $ drop (len + 1) arg

-- | Validate that a list of arguments are all of the same type
allSame :: [Arg] -> Bool
allSame []  = True
allSame [_] = True
allSame (a:b:rest) = a `sameAs` b && allSame (b:rest)
    where
        sameAs :: Arg -> Arg -> Bool
        sameAs (L _) (L _) = True
        sameAs (R _) (R _) = True
        sameAs _     _     = False

allLocal :: [Arg] -> Bool
allLocal = all local
    where
        local (L _) = True
        local _     = False

allRemote :: [Arg] -> Bool
allRemote = all remote
    where
        remote (R _) = True
        remote _     = False

-- | Process command line args genericly
handleArgs :: String                -- ^ a help message
           -> ([String] -> Maybe a) -- ^ a parser to make the list of 
                                    --   args into something useful
           -> (a -> IO ())          -- ^ what to do with that value 
                                    --   after it's parsed
           -> IO ()
handleArgs msg parser f = do
    args <- getArgs
    if helpFlagPresent args
        then putStrLn msg
        else case parser args of
            Just v -> f v
            _      -> putStrLn msg

    where
        helpFlagPresent :: [String] -> Bool
        helpFlagPresent []           = False
        helpFlagPresent ("-h":_)     = True
        helpFlagPresent ("--help":_) = True
        helpFlagPresent (_:rest)     = helpFlagPresent rest

-- | Get the AWS keys from the environment and execute the action with 
--   the connection. If the keys aren't set, error
withConnection :: (AWSConnection -> IO ()) -> IO ()
withConnection f = do
    maws <- amazonS3ConnectionFromEnv
    case maws of
        Just aws -> f aws
        _        -> errorEnvNotSet

-- | Either show the error or execute the action on the result
handleError :: AWSResult a -> (a -> IO ()) -> IO ()
handleError (Left e)  _ = hPutStrLn stderr $ prettyReqError e
handleError (Right v) f = f v

baseName :: FilePath -> FilePath
baseName = snd . splitFileName

stripPrefix :: String -> String -> String 
stripPrefix pref str =
    if pref `isPrefixOf` str
        then drop (length pref) str
        else str

addTrailingSlash :: FilePath -> FilePath
addTrailingSlash = reverse . addSlash . reverse
    where
        addSlash :: FilePath -> FilePath
        addSlash s@('/':_) = s
        addSlash s         = '/':s

stripLeadingSlash :: String -> String
stripLeadingSlash ('/':rest) = stripLeadingSlash rest
stripLeadingSlash x          = x

skip :: IOException -> IO ()
skip e = hPutStrLn stderr $ show e

errorInvalidArgs :: IO ()
errorInvalidArgs = hPutStrLn stderr "Invalid arguments for operation"

errorEnvNotSet :: IO ()
errorEnvNotSet = hPutStrLn stderr "AWS environment variables are not set"

-- | Taken from pandoc, Text.Pandoc.Shared
getMimeType :: FilePath -> String
getMimeType fp = M.findWithDefault "" (map toLower $ drop 1 $ takeExtension fp) mimeTypes
    where
        mimeTypes :: M.Map String String
        mimeTypes = M.fromList -- list from happstack
            [("gz","application/x-gzip")
            ,("cabal","application/x-cabal")
            ,("%","application/x-trash")
            ,("323","text/h323")
            ,("3gp","video/3gpp")
            ,("7z","application/x-7z-compressed")
            ,("abw","application/x-abiword")
            ,("ai","application/postscript")
            ,("aif","audio/x-aiff")
            ,("aifc","audio/x-aiff")
            ,("aiff","audio/x-aiff")
            ,("alc","chemical/x-alchemy")
            ,("art","image/x-jg")
            ,("asc","text/plain")
            ,("asf","video/x-ms-asf")
            ,("asn","chemical/x-ncbi-asn1")
            ,("aso","chemical/x-ncbi-asn1-binary")
            ,("asx","video/x-ms-asf")
            ,("atom","application/atom")
            ,("atomcat","application/atomcat+xml")
            ,("atomsrv","application/atomserv+xml")
            ,("au","audio/basic")
            ,("avi","video/x-msvideo")
            ,("b","chemical/x-molconn-Z")
            ,("bak","application/x-trash")
            ,("bat","application/x-msdos-program")
            ,("bcpio","application/x-bcpio")
            ,("bib","text/x-bibtex")
            ,("bin","application/octet-stream")
            ,("bmp","image/x-ms-bmp")
            ,("boo","text/x-boo")
            ,("book","application/x-maker")
            ,("bsd","chemical/x-crossfire")
            ,("c","text/x-csrc")
            ,("c++","text/x-c++src")
            ,("c3d","chemical/x-chem3d")
            ,("cab","application/x-cab")
            ,("cac","chemical/x-cache")
            ,("cache","chemical/x-cache")
            ,("cap","application/cap")
            ,("cascii","chemical/x-cactvs-binary")
            ,("cat","application/vnd.ms-pki.seccat")
            ,("cbin","chemical/x-cactvs-binary")
            ,("cbr","application/x-cbr")
            ,("cbz","application/x-cbz")
            ,("cc","text/x-c++src")
            ,("cdf","application/x-cdf")
            ,("cdr","image/x-coreldraw")
            ,("cdt","image/x-coreldrawtemplate")
            ,("cdx","chemical/x-cdx")
            ,("cdy","application/vnd.cinderella")
            ,("cef","chemical/x-cxf")
            ,("cer","chemical/x-cerius")
            ,("chm","chemical/x-chemdraw")
            ,("chrt","application/x-kchart")
            ,("cif","chemical/x-cif")
            ,("class","application/java-vm")
            ,("cls","text/x-tex")
            ,("cmdf","chemical/x-cmdf")
            ,("cml","chemical/x-cml")
            ,("cod","application/vnd.rim.cod")
            ,("com","application/x-msdos-program")
            ,("cpa","chemical/x-compass")
            ,("cpio","application/x-cpio")
            ,("cpp","text/x-c++src")
            ,("cpt","application/mac-compactpro")
            ,("crl","application/x-pkcs7-crl")
            ,("crt","application/x-x509-ca-cert")
            ,("csf","chemical/x-cache-csf")
            ,("csh","application/x-csh")
            ,("csm","chemical/x-csml")
            ,("csml","chemical/x-csml")
            ,("css","text/css")
            ,("csv","text/csv")
            ,("ctab","chemical/x-cactvs-binary")
            ,("ctx","chemical/x-ctx")
            ,("cu","application/cu-seeme")
            ,("cub","chemical/x-gaussian-cube")
            ,("cxf","chemical/x-cxf")
            ,("cxx","text/x-c++src")
            ,("d","text/x-dsrc")
            ,("dat","chemical/x-mopac-input")
            ,("dcr","application/x-director")
            ,("deb","application/x-debian-package")
            ,("dif","video/dv")
            ,("diff","text/x-diff")
            ,("dir","application/x-director")
            ,("djv","image/vnd.djvu")
            ,("djvu","image/vnd.djvu")
            ,("dl","video/dl")
            ,("dll","application/x-msdos-program")
            ,("dmg","application/x-apple-diskimage")
            ,("dms","application/x-dms")
            ,("doc","application/msword")
            ,("dot","application/msword")
            ,("dv","video/dv")
            ,("dvi","application/x-dvi")
            ,("dx","chemical/x-jcamp-dx")
            ,("dxr","application/x-director")
            ,("emb","chemical/x-embl-dl-nucleotide")
            ,("embl","chemical/x-embl-dl-nucleotide")
            ,("eml","message/rfc822")
            ,("ent","chemical/x-ncbi-asn1-ascii")
            ,("eps","application/postscript")
            ,("etx","text/x-setext")
            ,("exe","application/x-msdos-program")
            ,("ez","application/andrew-inset")
            ,("fb","application/x-maker")
            ,("fbdoc","application/x-maker")
            ,("fch","chemical/x-gaussian-checkpoint")
            ,("fchk","chemical/x-gaussian-checkpoint")
            ,("fig","application/x-xfig")
            ,("flac","application/x-flac")
            ,("fli","video/fli")
            ,("fm","application/x-maker")
            ,("frame","application/x-maker")
            ,("frm","application/x-maker")
            ,("gal","chemical/x-gaussian-log")
            ,("gam","chemical/x-gamess-input")
            ,("gamin","chemical/x-gamess-input")
            ,("gau","chemical/x-gaussian-input")
            ,("gcd","text/x-pcs-gcd")
            ,("gcf","application/x-graphing-calculator")
            ,("gcg","chemical/x-gcg8-sequence")
            ,("gen","chemical/x-genbank")
            ,("gf","application/x-tex-gf")
            ,("gif","image/gif")
            ,("gjc","chemical/x-gaussian-input")
            ,("gjf","chemical/x-gaussian-input")
            ,("gl","video/gl")
            ,("gnumeric","application/x-gnumeric")
            ,("gpt","chemical/x-mopac-graph")
            ,("gsf","application/x-font")
            ,("gsm","audio/x-gsm")
            ,("gtar","application/x-gtar")
            ,("h","text/x-chdr")
            ,("h++","text/x-c++hdr")
            ,("hdf","application/x-hdf")
            ,("hh","text/x-c++hdr")
            ,("hin","chemical/x-hin")
            ,("hpp","text/x-c++hdr")
            ,("hqx","application/mac-binhex40")
            ,("hs","text/x-haskell")
            ,("hta","application/hta")
            ,("htc","text/x-component")
            ,("htm","text/html")
            ,("html","text/html")
            ,("hxx","text/x-c++hdr")
            ,("ica","application/x-ica")
            ,("ice","x-conference/x-cooltalk")
            ,("ico","image/x-icon")
            ,("ics","text/calendar")
            ,("icz","text/calendar")
            ,("ief","image/ief")
            ,("iges","model/iges")
            ,("igs","model/iges")
            ,("iii","application/x-iphone")
            ,("inp","chemical/x-gamess-input")
            ,("ins","application/x-internet-signup")
            ,("iso","application/x-iso9660-image")
            ,("isp","application/x-internet-signup")
            ,("ist","chemical/x-isostar")
            ,("istr","chemical/x-isostar")
            ,("jad","text/vnd.sun.j2me.app-descriptor")
            ,("jar","application/java-archive")
            ,("java","text/x-java")
            ,("jdx","chemical/x-jcamp-dx")
            ,("jmz","application/x-jmol")
            ,("jng","image/x-jng")
            ,("jnlp","application/x-java-jnlp-file")
            ,("jpe","image/jpeg")
            ,("jpeg","image/jpeg")
            ,("jpg","image/jpeg")
            ,("js","application/x-javascript")
            ,("kar","audio/midi")
            ,("key","application/pgp-keys")
            ,("kil","application/x-killustrator")
            ,("kin","chemical/x-kinemage")
            ,("kml","application/vnd.google-earth.kml+xml")
            ,("kmz","application/vnd.google-earth.kmz")
            ,("kpr","application/x-kpresenter")
            ,("kpt","application/x-kpresenter")
            ,("ksp","application/x-kspread")
            ,("kwd","application/x-kword")
            ,("kwt","application/x-kword")
            ,("latex","application/x-latex")
            ,("lha","application/x-lha")
            ,("lhs","text/x-literate-haskell")
            ,("lsf","video/x-la-asf")
            ,("lsx","video/x-la-asf")
            ,("ltx","text/x-tex")
            ,("lyx","application/x-lyx")
            ,("lzh","application/x-lzh")
            ,("lzx","application/x-lzx")
            ,("m3u","audio/mpegurl")
            ,("m4a","audio/mpeg")
            ,("maker","application/x-maker")
            ,("man","application/x-troff-man")
            ,("mcif","chemical/x-mmcif")
            ,("mcm","chemical/x-macmolecule")
            ,("mdb","application/msaccess")
            ,("me","application/x-troff-me")
            ,("mesh","model/mesh")
            ,("mid","audio/midi")
            ,("midi","audio/midi")
            ,("mif","application/x-mif")
            ,("mm","application/x-freemind")
            ,("mmd","chemical/x-macromodel-input")
            ,("mmf","application/vnd.smaf")
            ,("mml","text/mathml")
            ,("mmod","chemical/x-macromodel-input")
            ,("mng","video/x-mng")
            ,("moc","text/x-moc")
            ,("mol","chemical/x-mdl-molfile")
            ,("mol2","chemical/x-mol2")
            ,("moo","chemical/x-mopac-out")
            ,("mop","chemical/x-mopac-input")
            ,("mopcrt","chemical/x-mopac-input")
            ,("mov","video/quicktime")
            ,("movie","video/x-sgi-movie")
            ,("mp2","audio/mpeg")
            ,("mp3","audio/mpeg")
            ,("mp4","video/mp4")
            ,("mpc","chemical/x-mopac-input")
            ,("mpe","video/mpeg")
            ,("mpeg","video/mpeg")
            ,("mpega","audio/mpeg")
            ,("mpg","video/mpeg")
            ,("mpga","audio/mpeg")
            ,("ms","application/x-troff-ms")
            ,("msh","model/mesh")
            ,("msi","application/x-msi")
            ,("mvb","chemical/x-mopac-vib")
            ,("mxu","video/vnd.mpegurl")
            ,("nb","application/mathematica")
            ,("nc","application/x-netcdf")
            ,("nwc","application/x-nwc")
            ,("o","application/x-object")
            ,("oda","application/oda")
            ,("odb","application/vnd.oasis.opendocument.database")
            ,("odc","application/vnd.oasis.opendocument.chart")
            ,("odf","application/vnd.oasis.opendocument.formula")
            ,("odg","application/vnd.oasis.opendocument.graphics")
            ,("odi","application/vnd.oasis.opendocument.image")
            ,("odm","application/vnd.oasis.opendocument.text-master")
            ,("odp","application/vnd.oasis.opendocument.presentation")
            ,("ods","application/vnd.oasis.opendocument.spreadsheet")
            ,("odt","application/vnd.oasis.opendocument.text")
            ,("oga","audio/ogg")
            ,("ogg","application/ogg")
            ,("ogv","video/ogg")
            ,("ogx","application/ogg")
            ,("old","application/x-trash")
            ,("otg","application/vnd.oasis.opendocument.graphics-template")
            ,("oth","application/vnd.oasis.opendocument.text-web")
            ,("otp","application/vnd.oasis.opendocument.presentation-template")
            ,("ots","application/vnd.oasis.opendocument.spreadsheet-template")
            ,("ott","application/vnd.oasis.opendocument.text-template")
            ,("oza","application/x-oz-application")
            ,("p","text/x-pascal")
            ,("p7r","application/x-pkcs7-certreqresp")
            ,("pac","application/x-ns-proxy-autoconfig")
            ,("pas","text/x-pascal")
            ,("pat","image/x-coreldrawpattern")
            ,("patch","text/x-diff")
            ,("pbm","image/x-portable-bitmap")
            ,("pcap","application/cap")
            ,("pcf","application/x-font")
            ,("pcf.Z","application/x-font")
            ,("pcx","image/pcx")
            ,("pdb","chemical/x-pdb")
            ,("pdf","application/pdf")
            ,("pfa","application/x-font")
            ,("pfb","application/x-font")
            ,("pgm","image/x-portable-graymap")
            ,("pgn","application/x-chess-pgn")
            ,("pgp","application/pgp-signature")
            ,("php","application/x-httpd-php")
            ,("php3","application/x-httpd-php3")
            ,("php3p","application/x-httpd-php3-preprocessed")
            ,("php4","application/x-httpd-php4")
            ,("phps","application/x-httpd-php-source")
            ,("pht","application/x-httpd-php")
            ,("phtml","application/x-httpd-php")
            ,("pk","application/x-tex-pk")
            ,("pl","text/x-perl")
            ,("pls","audio/x-scpls")
            ,("pm","text/x-perl")
            ,("png","image/png")
            ,("pnm","image/x-portable-anymap")
            ,("pot","text/plain")
            ,("ppm","image/x-portable-pixmap")
            ,("pps","application/vnd.ms-powerpoint")
            ,("ppt","application/vnd.ms-powerpoint")
            ,("prf","application/pics-rules")
            ,("prt","chemical/x-ncbi-asn1-ascii")
            ,("ps","application/postscript")
            ,("psd","image/x-photoshop")
            ,("py","text/x-python")
            ,("pyc","application/x-python-code")
            ,("pyo","application/x-python-code")
            ,("qt","video/quicktime")
            ,("qtl","application/x-quicktimeplayer")
            ,("ra","audio/x-pn-realaudio")
            ,("ram","audio/x-pn-realaudio")
            ,("rar","application/rar")
            ,("ras","image/x-cmu-raster")
            ,("rd","chemical/x-mdl-rdfile")
            ,("rdf","application/rdf+xml")
            ,("rgb","image/x-rgb")
            ,("rhtml","application/x-httpd-eruby")
            ,("rm","audio/x-pn-realaudio")
            ,("roff","application/x-troff")
            ,("ros","chemical/x-rosdal")
            ,("rpm","application/x-redhat-package-manager")
            ,("rss","application/rss+xml")
            ,("rtf","application/rtf")
            ,("rtx","text/richtext")
            ,("rxn","chemical/x-mdl-rxnfile")
            ,("sct","text/scriptlet")
            ,("sd","chemical/x-mdl-sdfile")
            ,("sd2","audio/x-sd2")
            ,("sda","application/vnd.stardivision.draw")
            ,("sdc","application/vnd.stardivision.calc")
            ,("sdd","application/vnd.stardivision.impress")
            ,("sdf","application/vnd.stardivision.math")
            ,("sds","application/vnd.stardivision.chart")
            ,("sdw","application/vnd.stardivision.writer")
            ,("ser","application/java-serialized-object")
            ,("sgf","application/x-go-sgf")
            ,("sgl","application/vnd.stardivision.writer-global")
            ,("sh","application/x-sh")
            ,("shar","application/x-shar")
            ,("shtml","text/html")
            ,("sid","audio/prs.sid")
            ,("sik","application/x-trash")
            ,("silo","model/mesh")
            ,("sis","application/vnd.symbian.install")
            ,("sisx","x-epoc/x-sisx-app")
            ,("sit","application/x-stuffit")
            ,("sitx","application/x-stuffit")
            ,("skd","application/x-koan")
            ,("skm","application/x-koan")
            ,("skp","application/x-koan")
            ,("skt","application/x-koan")
            ,("smi","application/smil")
            ,("smil","application/smil")
            ,("snd","audio/basic")
            ,("spc","chemical/x-galactic-spc")
            ,("spl","application/futuresplash")
            ,("spx","audio/ogg")
            ,("src","application/x-wais-source")
            ,("stc","application/vnd.sun.xml.calc.template")
            ,("std","application/vnd.sun.xml.draw.template")
            ,("sti","application/vnd.sun.xml.impress.template")
            ,("stl","application/vnd.ms-pki.stl")
            ,("stw","application/vnd.sun.xml.writer.template")
            ,("sty","text/x-tex")
            ,("sv4cpio","application/x-sv4cpio")
            ,("sv4crc","application/x-sv4crc")
            ,("svg","image/svg+xml")
            ,("svgz","image/svg+xml")
            ,("sw","chemical/x-swissprot")
            ,("swf","application/x-shockwave-flash")
            ,("swfl","application/x-shockwave-flash")
            ,("sxc","application/vnd.sun.xml.calc")
            ,("sxd","application/vnd.sun.xml.draw")
            ,("sxg","application/vnd.sun.xml.writer.global")
            ,("sxi","application/vnd.sun.xml.impress")
            ,("sxm","application/vnd.sun.xml.math")
            ,("sxw","application/vnd.sun.xml.writer")
            ,("t","application/x-troff")
            ,("tar","application/x-tar")
            ,("taz","application/x-gtar")
            ,("tcl","application/x-tcl")
            ,("tex","text/x-tex")
            ,("texi","application/x-texinfo")
            ,("texinfo","application/x-texinfo")
            ,("text","text/plain")
            ,("tgf","chemical/x-mdl-tgf")
            ,("tgz","application/x-gtar")
            ,("tif","image/tiff")
            ,("tiff","image/tiff")
            ,("tk","text/x-tcl")
            ,("tm","text/texmacs")
            ,("torrent","application/x-bittorrent")
            ,("tr","application/x-troff")
            ,("ts","text/texmacs")
            ,("tsp","application/dsptype")
            ,("tsv","text/tab-separated-values")
            ,("txt","text/plain")
            ,("udeb","application/x-debian-package")
            ,("uls","text/iuls")
            ,("ustar","application/x-ustar")
            ,("val","chemical/x-ncbi-asn1-binary")
            ,("vcd","application/x-cdlink")
            ,("vcf","text/x-vcard")
            ,("vcs","text/x-vcalendar")
            ,("vmd","chemical/x-vmd")
            ,("vms","chemical/x-vamas-iso14976")
            ,("vrm","x-world/x-vrml")
            ,("vrml","model/vrml")
            ,("vsd","application/vnd.visio")
            ,("wad","application/x-doom")
            ,("wav","audio/x-wav")
            ,("wax","audio/x-ms-wax")
            ,("wbmp","image/vnd.wap.wbmp")
            ,("wbxml","application/vnd.wap.wbxml")
            ,("wk","application/x-123")
            ,("wm","video/x-ms-wm")
            ,("wma","audio/x-ms-wma")
            ,("wmd","application/x-ms-wmd")
            ,("wml","text/vnd.wap.wml")
            ,("wmlc","application/vnd.wap.wmlc")
            ,("wmls","text/vnd.wap.wmlscript")
            ,("wmlsc","application/vnd.wap.wmlscriptc")
            ,("wmv","video/x-ms-wmv")
            ,("wmx","video/x-ms-wmx")
            ,("wmz","application/x-ms-wmz")
            ,("wp5","application/wordperfect5.1")
            ,("wpd","application/wordperfect")
            ,("wrl","model/vrml")
            ,("wsc","text/scriptlet")
            ,("wvx","video/x-ms-wvx")
            ,("wz","application/x-wingz")
            ,("xbm","image/x-xbitmap")
            ,("xcf","application/x-xcf")
            ,("xht","application/xhtml+xml")
            ,("xhtml","application/xhtml+xml")
            ,("xlb","application/vnd.ms-excel")
            ,("xls","application/vnd.ms-excel")
            ,("xlt","application/vnd.ms-excel")
            ,("xml","application/xml")
            ,("xpi","application/x-xpinstall")
            ,("xpm","image/x-xpixmap")
            ,("xsl","application/xml")
            ,("xtel","chemical/x-xtel")
            ,("xul","application/vnd.mozilla.xul+xml")
            ,("xwd","image/x-xwindowdump")
            ,("xyz","chemical/x-xyz")
            ,("zip","application/zip")
            ,("zmt","chemical/x-mopac-input")
            ]
