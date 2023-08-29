{-# LANGUAGE OverloadedLists #-}

module Test.Gibberish.GenTrigraphTest (tests) where

import Data.Gibberish.GenTrigraph
import Data.Gibberish.Types
import Test.Gibberish.Gen qualified as Gen

import Data.List qualified as List
import Data.Map qualified as Map
import Data.Text (Text ())
import Data.Text qualified as Text
import Hedgehog
import Hedgehog.Gen qualified as Gen hiding (word)
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree (), testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Test.Gibberish.GenTrigraph"
    [ testPropertyNamed "length trigraph" "prop_len_trigraph" prop_len_trigraph,
      testPropertyNamed "length frequencies" "prop_len_frequencies" prop_len_frequencies,
      testPropertyNamed "contains all trigrams" "prop_trigrams_all" prop_trigrams_all
    ]

prop_len_trigraph :: Property
prop_len_trigraph = property $ do
  word <- forAll Gen.word
  let trigrams' = List.sort $ trigrams word

  cover 10 "with duplicates" $ hasDuplicates trigrams'
  cover 10 "no duplicates" $ not (hasDuplicates trigrams')

  assert $
    length (unTrigraph $ genTrigraph [word]) <= max 0 (Text.length word - 2)

prop_len_frequencies :: Property
prop_len_frequencies = property $ do
  word <- forAll Gen.word
  let trigrams' = List.sort $ trigrams word

  cover 10 "with duplicates" $ hasDuplicates trigrams'
  cover 10 "no duplicates" $ not (hasDuplicates trigrams')

  let totalTrigrams =
        sum
          . concatMap (Map.elems . unFrequencies)
          . Map.elems
          . unTrigraph
          . genTrigraph
          $ [word]
  length trigrams' === fromIntegral totalTrigrams

prop_trigrams_all :: Property
prop_trigrams_all = property $ do
  words' <- forAll $ Gen.list (Range.linear 0 10) Gen.word
  let trigraphs = genTrigraph words'
      trigrams' = map (List.sort . trigrams) words'

  cover 10 "with duplicates" $ List.any hasDuplicates trigrams'
  cover 10 "no duplicates" $ not (List.all hasDuplicates trigrams')

  concatNub trigrams' === List.sort (allTrigrams trigraphs)
  where
    concatNub :: Ord a => [[a]] -> [a]
    concatNub = List.nub . List.sort . List.concat

trigrams :: Text -> [Trigram]
trigrams ts = case Text.take 3 ts of
  [a, b, c] -> Trigram a b c : trigrams (Text.tail ts)
  _ -> []

allTrigrams :: Trigraph -> [Trigram]
allTrigrams (Trigraph tris) =
  concatMap (uncurry mapFrequencies) (Map.toList tris)
  where
    mapFrequencies (Digram c1 c2) (Frequencies freqs) =
      map (\(Unigram c3) -> Trigram c1 c2 c3) $ Map.keys freqs

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates ls = ls /= List.nub ls
