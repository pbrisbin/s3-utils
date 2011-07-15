# S3cp

`scp`-like access to your Amazon S3 storage.

### Configuration

Sign up for S3.

Export the environment variables `AWS_ACCESS_KEY_ID` and 
`AWS_SECRET_ACCESS_KEY`.

### Installation

~~~ 
cabal install hs3
cabal install wai-app-static

git clone git://github.com/pbrisbin/s3cp.git ./s3cp

cd ./s3cp && ghc --make -o s3cp Main.hs
~~~

`mv ./s3` into `$PATH` or always call it qualified.

### Usage

~~~ 
# local file(s) -> S3
s3cp <path> ... bucket:path

# S3 -> local file
s3cp bucket:path <path>
~~~

### Note

It's not really working yet...

### Thanks

Greg Heartsfield for [hs3][] and Michael Snoyman for [wai-app-static][].

[hs3]:            http://hackage.haskell.org/package/hs3
[wai-app-static]: http://hackage.haskell.org/package/wai-app-static