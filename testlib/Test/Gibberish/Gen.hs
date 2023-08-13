module Test.Gibberish.Gen
  ( trigram,
    digram,
    unigram,
    frequencies,
    frequency,
  ) where

import Data.Elocrypt.Types

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

trigram :: Gen Trigram
trigram = do
  Trigram <$> Gen.map (Range.linear 0 250) kv
  where
    kv = (,) <$> digram <*> frequencies

digram :: Gen Digram
digram = Digram <$> Gen.unicode <*> Gen.unicode

unigram :: Gen Unigram
unigram = Unigram <$> Gen.unicode

frequencies :: Gen Frequencies
frequencies =
  Frequencies
    <$> Gen.map (Range.linear 0 25) kv
  where
    kv = (,) <$> unigram <*> frequency

frequency :: Gen Frequency
frequency = Frequency <$> Gen.int (Range.linear 0 maxBound)
