> [!NOTE]
> All of my GitHub repositories have been **archived** and will be migrated to
> Codeberg as I next work on them. This repository either now lives, or will
> live, at:
>
> https://codeberg.org/pbrisbin/s3-utils
>
> If you need to report an Issue or raise a PR, and this migration hasn't
> happened yet, send an email to me@pbrisbin.com.

# S3-utils

Core-utils for your S3 instance.

* `s3ls` - list remote buckets, directories, or files
* `s3cp` - upload/download/copy directories or files
* `s3mv` - move remote directories or files
* `s3rm` - remove remote buckets, directories, or files

## Installation

~~~ 
git clone git://github.com/pbrisbin/s3-utils.git
cd ./s3-utils && cabal install
~~~

Export `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY`.

## Usage

See the `--help`.

## Notes

This package also installs its module: `Network.AWS.Utils`. It is well 
[documented][here].

[here]: http://pbrisbin.com/haskell/docs/html/s3-utils/Network-AWS-Utils.html

## You have been warned

It's working well in simple tests, but these tools are still very alpha. 
Beware bugs and don't use with important data.
