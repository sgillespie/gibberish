module Data.Gibberish
  ( -- * Generating Passwords
    genPassword,
    genPasswords,
    genPasswords',

    -- * Generating Passphrases
    genPassphrase,
    genPassphrase',

    -- * The Pass Monad
    Pass (),
    runPass,
    evalPass,
    usingPass,

    -- * The PassT Monad Transformer
    PassT (..),
    runPassT,
    evalPassT,
    usingPassT,
    module Control.Monad.Random,

    -- * Core Types
    GenPasswordOpts (..),
    GenPassphraseOpts (..),
    Language (..),
    TrigraphConfig (..),
    Trigraph (..),
    Word (..),
    Unigram (..),
    Digram (..),
    Trigram (..),
    Frequency (..),
    Frequencies (..),

    -- * Error Handling
    GibberishErr (..),
    isTrigraphNotFound,
    isImpossibleError,

    -- * Working with Trigraphs
    genTrigraph,
    loadTrigraph,
  ) where

import Data.Gibberish.Errors
import Data.Gibberish.Gen
import Data.Gibberish.Monad.Pass
import Data.Gibberish.Types

import Control.Monad.Random
import Prelude hiding (Word ())
