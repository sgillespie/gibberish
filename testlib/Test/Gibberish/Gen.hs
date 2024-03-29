module Test.Gibberish.Gen
  ( trigraph,
    digram,
    unigram,
    frequencies,
    frequency,
    word,
    genPasswordOpts,
    genPassphraseOpts,
    stdGen,
  ) where

import Data.Gibberish.Types

import Control.Monad.Random (StdGen (), mkStdGen)
import Data.Text (Text ())
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

trigraph :: Gen Trigraph
trigraph = do
  Trigraph <$> Gen.map (Range.linear 0 250) kv
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

word :: Gen Text
word = Gen.text (Range.linear 3 30) $ Gen.enum 'a' 'e'

genPasswordOpts :: Gen GenPasswordOpts
genPasswordOpts =
  GenPasswordOpts
    <$> Gen.bool
    <*> Gen.bool
    <*> Gen.bool
    <*> trigraph
    <*> Gen.int (Range.linear 0 15)

genPassphraseOpts :: Gen GenPassphraseOpts
genPassphraseOpts = do
  minLen <- Gen.int (Range.linear 0 10)
  maxLen <- Gen.int (Range.linear minLen 15)

  GenPassphraseOpts
    <$> Gen.bool
    <*> Gen.bool
    <*> Gen.bool
    <*> trigraph
    <*> pure minLen
    <*> pure maxLen

stdGen :: Gen StdGen
stdGen = mkStdGen <$> Gen.integral (Range.linear minBound maxBound)
