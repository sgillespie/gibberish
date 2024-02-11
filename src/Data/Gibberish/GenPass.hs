{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Gibberish.GenPass
  ( genPassword,
  ) where

import Data.Gibberish.MonadPass (MonadRandom ())
import Data.Gibberish.Types
import Data.Gibberish.Utils

import Control.Arrow ((>>>))
import Control.Monad ((>=>))
import Control.Monad.Random (MonadRandom (..), fromList, fromListMay, uniform)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe)
import Data.Bifunctor (bimap, second)
import Data.Char (toLower, toUpper)
import Data.Map (Map ())
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ratio
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
  -- Select the first two characters
  f2 <- first2 opts
  -- Select the rest of the characters
  rest <- lastN opts (optsLength - 2) f2
  -- Construct the full password from f2 and rest
  let pass = digramToText f2 `Text.append` Text.reverse rest

  -- Apply transformations in order
  let transform =
        Text.map toLower
          >>> capitalize opts
          >=> digitize opts

  Word <$> transform pass

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

-- | Randomly capitalize at least 1 character. Additional characters capitalize
--  at a probability of 1/12
capitalize :: MonadRandom m => GenPassOptions -> Text -> m Text
capitalize opts@GenPassOptions {..} t
  | optsCapitals = capitalizeR =<< capitalize1 opts t
  | otherwise = pure t

-- | Randomly capitalize 1 character
capitalize1 :: MonadRandom m => GenPassOptions -> Text -> m Text
capitalize1 GenPassOptions {..} t =
  update1 (pure . toUpper) t =<< getRandomR (0, optsLength - 1)

capitalizeR :: MonadRandom m => Text -> m Text
capitalizeR = updateR (pure . toUpper) (1 % 12)

digitize :: MonadRandom m => GenPassOptions -> Text -> m Text
digitize opts t
  | optsDigits opts = digitizeR =<< digitize1 opts t
  | otherwise = pure t

digitize1 :: MonadRandom m => GenPassOptions -> Text -> m Text
digitize1 _ t
  | null candidates = pure t
  | otherwise = digitize1' =<< uniform candidates
  where
    candidates = findIndices (`elem` Map.keys numeralConversions) t
    digitize1' = update1 (uniform . toDigit) t

digitizeR :: MonadRandom m => Text -> m Text
digitizeR = updateR (uniform . toDigit) (1 % 6)

-- | Map a letter to one or more digits, if possible
toDigit :: Char -> [Char]
toDigit c = fromMaybe [c] (numeralConversions Map.!? c)
