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
genPassword opts@GenPassOptions {..}
  | optsLength <= 2 = Word . Text.take optsLength . digramToText <$> first2 opts
  | otherwise = genPassword' opts

-- | Generates a password with the given options. Assumes optsLength is at least 3.
genPassword' :: MonadRandom m => GenPassOptions -> m Word
genPassword' opts@(GenPassOptions {..}) = do
  f2 <- first2 opts
  rest <- lastN opts (optsLength - 2) f2

  let pass = digramToText f2 `Text.append` Text.reverse rest

  pure (Word pass)

digramToText :: Digram -> Text
digramToText (Digram a b) = [a, b]

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
lastN opts len di@(Digram _ b)
  | len <= 0 = pure []
  | otherwise = do
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
