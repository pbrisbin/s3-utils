# S3

Simple push/pull access to your Amazon S3 storage.

### Goal

The eventual goal is to provide a smart off-site backup tool utilizing 
Amazon's S3 cloud storage. This tool would need to behave like `rsync` 
and have a focus on cutting unnecessary data transfer (since you pay by 
the byte).

Currently one can use this tool to push files or directories up to a 
bucket or pull files down from a bucket.

### Configuration

For now the project uses configuration by compilation. That means you'll 
need to create a `Config.hs` before compiling the executable (mine is 
hidden from the repo).

Here is an example:

~~~ { .haskell }
module Config
    ( myConnection
    , myBucket
    ) where

import Network.AWS.AWSConnection

myConnection :: AWSConnection
myConnection = amazonS3Connection "<your key>" "<your secret key>"

myBucket :: String
myBucket = "<some bucket>"
~~~

### Installation

The following packages are required (both available on hackage):

~~~ 
cabal install hs3
cabal install wai-app-static
~~~

`wai-app-static` is only used for its `defaultMimeTypeByExt` function 
(for now).

Once you setup `Config.hs`, compile:

~~~ 
ghc --make -o s3 Main.hs
~~~

*Feel free to choose a different name for the binary*

`mv ./s3` into `$PATH` or always call it qualified.

### Usage

Currently, there are two ways to use `s3`: *send* is the default and 
*get* is used when you pass `--get` as the first argument.

~~~ 
usage: s3 [ --get ] <path> ...
~~~

You can pass as many `<path>` arguments as you would like. *Get*ting 
directories is not supported yet (but *send*ing them is).

In both cases, the S3 side of the transaction will use a stripped 
filename while the filesystem side is always written/read exactly as it 
was passed.

Here are some examples to clarify:

~~~ 
Path        Read-written on filesystem   Read-written on S3
-----      ---------------------------  -------------------
./foo                            ./foo         myBucket/foo
../bar                          ../bar         myBucket/bar
/some/dir                    /some/dir    myBucket/some/dir
~~~

### Thanks

Greg Heartsfield for [hs3][] and Michael Snoyman for [wai-app-static][].

[hs3]:            http://hackage.haskell.org/package/hs3
[wai-app-static]: http://hackage.haskell.org/package/wai-app-static
