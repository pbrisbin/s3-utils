# S3-utils

core-utils for your S3 instance

### Provided

Done:

~~~ 
usage: s3cp <path> ... bucket:[<path>]
       s3cp <bucket:path> ... <path>  
~~~

Todo:

* `s3ls`
* `s3mv`
* `s3rm`

### Installation

~~~ 
git clone git://github.com/pbrisbin/s3-utils.git
cd ./s3-utils && cabal install
~~~

### Configuration

Export the environment variables `AWS_ACCESS_KEY_ID` and 
`AWS_SECRET_ACCESS_KEY`.

### Note

Very alpha. Beware bugs. Don't use on important data.
