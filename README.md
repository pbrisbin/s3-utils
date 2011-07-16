# S3-utils

Core-utils for your S3 instance.

### Installation

~~~ 
git clone git://github.com/pbrisbin/s3-utils.git
cd ./s3-utils && cabal install
~~~

### Configuration

Export the environment variables `AWS_ACCESS_KEY_ID` and 
`AWS_SECRET_ACCESS_KEY`.

### Usage

Downloading files: `s3cp <bucket:path> ... <path>`

Uploading files: `s3cp <path> ... bucket:[<path>]`

Uploading data from a stream: `command | s3put <bucket:path>`

Moving remote files/buckets: `s3mv <bucket:[path]> ... <bucket:[path]>` *TODO*

Removing remote files/buckets: `s3rm <bucket:[path]> ...`

Listing remote files/buckets: `s3ls <bucket:[path]> ...`

### Notes

All commands print the files they're affecting; redirect to `/dev/null` 
to silence this.

All commands act recursively on directories (remote and local) but all 
actions are (in the end) executed on a file-by-file basis.

Any failures (permissions, connection, etc) will cause that file to be 
skipped -- processing continues. This does not apply to errors due to 
*invalid usage* or *unset environment variables*.

In addition to the binaries, this package also installs its module: 
`Network.AWS.Utils`.

### You have been warned

It's working well in simple tests, but these tools are still very alpha. 
Beware bugs and don't use with important data.
