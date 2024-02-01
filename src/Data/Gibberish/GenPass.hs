{-# LANGUAGE OverloadedLists #-}

module Data.Gibberish.GenPass
  ( genPassword,
  ) where

import Data.Gibberish.MonadPass (MonadRandom ())
import Data.Gibberish.Types

import Control.Monad.Random (fromList, fromListMay, uniform)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe)
import Data.Bifunctor (bimap, second)
import Data.Map qualified as Map
import Data.Text (Text ())
import Data.Text qualified as Text
import Prelude hiding (Word)

-- | Generate a password with the given options
genPassword :: MonadRandom m => GenPassOptions -> m Word
genPassword opts@GenPassOptions {..} = do
  (Digram f1 f2) <- first2 opts

  pass <-
    if optsLength > 2
      then
        lastN opts (optsLength - 2) (Digram f1 f2) >>= \l -> pure $ f1 `Text.cons` f2 `Text.cons` Text.reverse l
      else pure (Text.take optsLength [f1, f2])

  pure (Word pass)

first2 :: MonadRandom m => GenPassOptions -> m Digram
first2 GenPassOptions {optsTrigraph = Trigraph trigraph} =
  fromList . map toWeight . Map.toList $ trigraph
  where
    toWeight :: (Digram, Frequencies) -> (Digram, Rational)
    toWeight = second sumFrequencies

    sumFrequencies :: Frequencies -> Rational
    sumFrequencies (Frequencies freqs) =
      Map.foldr (\a b -> fromIntegral a + b) 0 freqs

lastN :: MonadRandom m => GenPassOptions -> Int -> Digram -> m Text
lastN _ 0 _ = pure []
lastN opts len di@(Digram _ b) = do
  c <- next opts di
  rs <- lastN opts (len - 1) (Digram b c)
  pure (c `Text.cons` rs)

next :: MonadRandom m => GenPassOptions -> Digram -> m Char
next GenPassOptions {..} digram = do
  res <- runMaybeT $ do
    (Frequencies freqs) <- hoistMaybe $ Map.lookup digram (unTrigraph optsTrigraph)
    let weights = map (bimap unUnigram fromIntegral) (Map.toList freqs)
    MaybeT $ fromListMay weights

  -- If there are no suitable candidates, choose one at random
  maybe nextDefault pure res

nextDefault :: MonadRandom m => m Char
nextDefault = uniform (['a' .. 'z'] :: [Char])
