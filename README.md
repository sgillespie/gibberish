# Gibberish
![Build Status](https://github.com/sgillespie/gibberish/actions/workflows/build.yaml/badge.svg)

Gibberish generates pronounceable passwords that are easy-to-remember and hard-to-guess. Gibberish
can also generate pseudo english passphrases.

## Prerequisites
In order to build or install you will need
 * GHC (tested on 8.6.5)
 * Stack (tested on 2.1.3.1)

## Installing
Gibberish is on [Hackage](https://hackage.haskell.org/package/elocrypt).  Installation is as easy as:
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

Gibberish sources can be found 
 * https://github.com/sgillespie/gibberish
 * https://hackage.haskell.org/package/elocrypt

## Building

In order to build or install you will need
 * [GHC](https://www.haskell.org/ghc) (tested on 8.6.5)
 * [Haskell Stack](https://haskellstack.org) (tested on 2.1.3.1)

Build gibberish:
```
stack setup
stack build
```
Then, install (if desired):
```
stack install
```

## API Documentation

The full API documentation is on hackage @
https://hackage.haskell.org/package/elocrypt/docs. To build the documentation yourself,
run

```
stack haddock
```

### API Examples

You can use gibberish to generate words in any Haskell code, so long as you have installed
gibberish. Generate a word by using Data.Elocrypt.newPassword

```
import Data.Elocrypt
...
-- Generate a word of length 10
fooGen :: IO String
fooGen = newPassword 10 `liftM` getStdGen
```

Alternatively, you can use Data.Elocrypt.mkPassword if you want to complete control of the
random monad

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

## Acknowledgements

Gibberish is based on Tom Van Vleck's work on 3rd order approximation to english for
generating passwords. Gibberish is based on his [javascript
generator](http://www.multicians.org/thvv/gpw-js.html).

## LICENSE

Gibberish is licensed under the MIT license. Please see [LICENSE](LICENSE) for details
