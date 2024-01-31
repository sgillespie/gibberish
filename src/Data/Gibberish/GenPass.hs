{-# LANGUAGE OverloadedLists #-}

module Data.Gibberish.GenPass
  ( genPassword,
  ) where

import Data.Gibberish.MonadPass (MonadRandom ())
import Data.Gibberish.Types

import Control.Monad.Random (fromList)
import Data.Bifunctor (bimap)
import Data.Map qualified as Map
import Data.Text (Text ())
import Data.Text qualified as Text
import Prelude hiding (Word)

-- | Generate a password with the given options
genPassword :: MonadRandom m => GenPassOptions -> m Word
genPassword opts@GenPassOptions {..} = do
  f2 <- first2 opts
  let f2' = Text.reverse f2

  pass <-
    if optsLength > 2
      then lastN (optsLength - 2) f2'
      else pure (Text.take optsLength f2')
  let pass' = Text.reverse pass

  pure (Word pass')

first2 :: MonadRandom m => GenPassOptions -> m Text
first2 GenPassOptions {optsTrigraph = Trigraph trigraph} =
  fromList . map toWeight . Map.toList $ trigraph
  where
    toWeight :: (Digram, Frequencies) -> (Text, Rational)
    toWeight = bimap digramToText sumFrequencies

    digramToText :: Digram -> Text
    digramToText (Digram c1 c2) = [c1, c2]

    sumFrequencies :: Frequencies -> Rational
    sumFrequencies (Frequencies freqs) =
      Map.foldr (\a b -> fromIntegral a + b) 0 freqs

lastN :: MonadRandom m => Int -> Text -> m Text
lastN 0 ts = pure ts
lastN len ts = lastN (len - 1) ('a' `Text.cons` ts)
