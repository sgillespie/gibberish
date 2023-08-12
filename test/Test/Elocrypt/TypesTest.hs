module Test.Elocrypt.TypesTest (tests) where

import Data.Elocrypt.Types

import Data.Aeson (fromJSON, toJSON)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree (), testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Test.Elocrypt.Types"
    [testPropertyNamed "toJSON roundtrip" "prop_toJSON_roundtrip" prop_toJSON_roundtrip]

prop_toJSON_roundtrip :: Property
prop_toJSON_roundtrip = property $ do
  unigram <- forAll genTrigram
  tripping unigram toJSON fromJSON

genTrigram :: Gen Trigram
genTrigram = do
  Trigram <$> Gen.map (Range.linear 0 250) kv
  where
    kv = (,) <$> genDigram <*> genFrequencies

genDigram :: Gen Digram
genDigram = Digram <$> Gen.unicode <*> Gen.unicode

genFrequencies :: Gen Frequencies
genFrequencies =
  Frequencies
    <$> Gen.map (Range.linear 0 25) kv
  where
    kv = (,) <$> genUnigram <*> genFrequency

genUnigram :: Gen Unigram
genUnigram = Unigram <$> Gen.unicode

genFrequency :: Gen Frequency
genFrequency = Frequency <$> Gen.int (Range.linear 0 maxBound)
