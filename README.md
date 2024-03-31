# Gibberish
![Build Status](https://github.com/sgillespie/gibberish/actions/workflows/build.yaml/badge.svg)

Gibberish generates pronounceable passwords that are easy-to-remember and hard-to-guess. Gibberish
can also generate pseudo english passphrases.

## Prerequisites

In order to build Gibberish, you will need

 * [GHC](https://www.haskell.org/downloads/) (tested on 9.6.2), _or_
 * [Nix](https://nixos.org/download/) (tested on 2.18.1)

## Installing

Gibberish is on [Hackage](https://hackage.haskell.org/package/gibberish). The `gibber`
executable can be installed with _cabal-install_:

    cabal install gibberish

Alternatively, binary distributions are available for x86_64 linux and windows:

 * [gibberish-3.0.0.0-x86_64-linux.tar.gz](https://github.com/sgillespie/gibberish/releases/download/v3.0.0.0/gibberish-3.0.0.0-x86_64-linux.tar.gz)
 * [gibberish-3.0.0.0-x86_64-windows.zip](https://github.com/sgillespie/gibberish/releases/download/v3.0.0.0/gibberish-3.0.0.0-x86_64-windows.zip)

Unfortunately I don't have access to macOS or aarch64. If you can help me build binaries
for those platforms, please open an issue!

## Building

The `gibber` executable can be built with _cabal-install_:

    cabal build

On Nix, the it can be built with:

    nix build

## Running

Genarate a list of passwords:

    gibber <length>

Generate random phrases (1 per line):

    gibber --passphrases <min-length> <max-length>

To see all available options:

    gibber --help

## API Documentation

The full API documentation is on hackage at
https://hackage.haskell.org/package/gibberish/docs. The documentation can also be built with:

    cabal haddock

## API Example

Gibberish can be used to generate words or phrases in Haskell code. For example: generate
a word by using `Data.Gibberish`:


    import Data.Gibberish
    import Data.Text (Text ())
    ...
    -- Generate a word of length 10
    fooGen :: IO Text
    fooGen = do
      gen <- getStdGen
      trigraph <- loadTrigraph English

      let opts =
          GenPasswordOpts
            { woptsCapitals = False,
              woptsDigits = False,
              woptsSpecials = False,
              woptsTrigraph = trigraph,
              woptsLength = 10
            }

      let (word, _) = usingPass gen (genPassword opts)
      pure (unWord word)

## Authors
Sean Gillespie <sean@mistersg.net>

## Acknowledgements

Gibberish is based on Tom Van Vleck's work on 3rd order approximation to english for
generating passwords. Gibberish is based on his [javascript
generator](http://www.multicians.org/thvv/gpw-js.html).

## LICENSE

Gibberish is licensed under the MIT license. Please see [LICENSE](LICENSE) for details
