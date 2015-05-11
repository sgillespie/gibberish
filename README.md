# Elocrypt
[![Build Status](https://travis-ci.org/sgillespie/elocrypt.svg?branch=master)](https://travis-ci.org/sgillespie/elocrypt)

Elocrypt is a Haskell library that generates pronounceable, hard-to-guess passwords.. like, harder than Vince Carter's need cartilage. Elocrypt includes a library and program.

## Downloading
Elocrypt sources can be found @ https://github.com/sgillespie/elocrypt

## Building
In order to build or install you will need
 * GHC (tested on 7.10 and 7.8)
 * cabal-install (tested on 1.20 and 1.22)

Build elocrypt:
```
cabal configure
cabal build
```
Then, install (if desired):
```
cabal install
```

## Running
Running elocrypt is as simple as...
```
elocrypt [length]
```

## Authors
Sean Gillespie <sean@mistersg.net>

## Credits
Thanks to Tom Van Vleck for creating a 3rd order english approximation password generator.  Elocrypt is based on his javascript generator @ http://www.multicians.org/thvv/gpw-js.html

## Copying
You can use this source for any reason, provided that you:

 * Let Tom Van Vleck know you are using it: http://www.multicians.org/cgi-sys/cgiwrap/thvv/squirnet.cgi
 * Share your source freely
 * Give Tom Van Vleck, and all the other pioneers, if you use the data or algorithms. (A link to http://www.multicians.org/thvv/gpw-js.html is sufficient
 * Use a compatible license

Please see LICENSE for all details
