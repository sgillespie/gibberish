module Data.Gibberish
  ( -- * Core types
    GenPasswordOpts (..),
    GenPassphraseOpts (..),
    Unigram (..),
    Digram (..),
    Trigram (..),
    Frequency (..),
    Frequencies (..),
    Trigraph (..),
    Word (..),

    -- * Password/phrase generation
    genPassword,
    genPasswords,
    genPasswords',
    genPassphrase,
    genPassphrase',

    -- * Trigraph generation
    Language (..),
    TrigraphConfig (..),
    genTrigraph,
    loadTrigraph,

    -- * Password/phrase generation monad
    Pass (),
    PassT (..),
    MonadRandom (..),
    runPass,
    evalPass,
    usingPass,
    runPassT,
    evalPassT,
    usingPassT,

    -- * Error handling
    GibberishErr (..),
    isTrigraphNotFound,
    isImpossibleError,
  ) where

import Data.Gibberish.Errors
import Data.Gibberish.Gen
import Data.Gibberish.Monad.Pass
import Data.Gibberish.Types

import Prelude hiding (Word ())
