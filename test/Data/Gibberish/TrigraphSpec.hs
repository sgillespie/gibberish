{-# LANGUAGE OverloadedLists #-}

module Data.Gibberish.TrigraphSpec (spec) where

import Data.Gibberish.Trigraph
import Data.Gibberish.Types
import Paths_gibberish (getDataFileName)
import Test.Gibberish.Gen qualified as Gen

import Control.Monad (void)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Text (Text ())
import Data.Text qualified as Text
import Hedgehog
import Hedgehog.Gen qualified as Gen hiding (word)
import Hedgehog.Range qualified as Range
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = do
  describe "genTrigraph" $ do
    it "length trigraph <= length word - 2" $ hedgehog $ do
      word <- forAll Gen.word
      let trigrams' = List.sort $ trigrams word

      cover 10 "with duplicates" $ hasDuplicates trigrams'
      cover 10 "no duplicates" $ not (hasDuplicates trigrams')

      assert $
        length (unTrigraph $ genTrigraph [word]) <= max 0 (Text.length word - 2)

    it "sum frequencies = length trigrams" $ hedgehog $ do
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

    it "allTrigrams trigraphs = nub trigrams" $ hedgehog $ do
      words' <- forAll $ Gen.list (Range.linear 0 10) Gen.word
      let trigraphs = genTrigraph words'
          trigrams' = map (List.sort . trigrams) words'

      cover 10 "with duplicates" $ List.any hasDuplicates trigrams'
      cover 10 "no duplicates" $ not (List.all hasDuplicates trigrams')

      concatNub trigrams' === List.sort (allTrigrams trigraphs)

  describe "loadTrigraph" $ do
    it "loads english" $
      void $
        loadTrigraph English

    it "loads custom" $ do
      tri <- getDataFileName ("data" </> "trigraphs" </> "wamerican.json")
      void $
        loadTrigraph (CustomTrigraph $ TrigraphConfig tri)

    it "handles load failure" $ do
      loadTrigraph (CustomTrigraph $ TrigraphConfig "doesnotexist.json")
        `shouldThrow` isTrigraphNotFound

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

concatNub :: Ord a => [[a]] -> [a]
concatNub = List.nub . List.sort . List.concat
