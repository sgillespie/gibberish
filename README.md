# Elocrypt
[![Build Status](https://travis-ci.org/sgillespie/elocrypt.svg?branch=master)](https://travis-ci.org/sgillespie/elocrypt)

Elocrypt generates pronounceable, easy-to-remember, hard-to-guess passwords... as hard as Vince Carter's knee cartilage is. Elocrypt includes a Haskell library and program.

## Prerequisites
In order to build or install you will need
 * GHC (tested on 8.6.5)
 * Stack (tested on 2.1.3.1)

## Installing
Elocrypt is on [Hackage](https://hackage.haskell.org/package/elocrypt).  Installation is as easy as:
```
cabal install elocrypt
```

Binaries are also available:
 * [elocrypt-2.1.0-linux-bin.tar.gz](https://github.com/sgillespie/elocrypt/releases/download/v2.1.0/elocrypt-v2.1.0-linux-bin.tar.gz)
 * [elocrypt-2.1.0-osx-bin.exe](https://github.com/sgillespie/elocrypt/releases/download/v2.1.0/elocrypt-v2.1.0-osx-bin.tar.gz)

## Running
Generate a list passwords:
```
elocrypt [length]
```

You can also generate random phrases (1 per line):
```
elocrypt --passphrases [min-length] [max-length]
```

## Obtaining the source
Elocrypt sources can be found 
 * https://github.com/sgillespie/elocrypt
 * https://hackage.haskell.org/package/elocrypt

## Building
In order to build or install you will need
 * [GHC](https://www.haskell.org/ghc) (tested on 8.6.5)
 * [Haskell Stack](https://haskellstack.org) (tested on 2.1.3.1)

Build elocrypt:
```
stack setup
stack build
```
Then, install (if desired):
```
stack install
```

## API Documentation
The full API documentation is on hackage @ https://hackage.haskell.org/package/elocrypt/docs. To build the documentation yourself, run
```
stack haddock
```

### API Examples
You can use elocrypt to generate words in any Haskell code, so long as you have installed elocrypt. Generate a word by using Data.Elocrypt.newPassword
```
import Data.Elocrypt
...
-- Generate a word of length 10
fooGen :: IO String
fooGen = newPassword 10 `liftM` getStdGen
```

Alternatively, you can use Data.Elocrypt.mkPassword if you want to complete control of the random monad
```
import Data.Elocrypt
import Control.Monad.Random
...
-- Generate a word of length 10
fooGen' :: IO String
fooGen' = evalRand (mkPassword 10) `liftM` getStdGen
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
