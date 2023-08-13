module Test.Elocrypt.TypesTest (tests) where

import Test.Gibberish.Gen qualified as Gen

import Data.Aeson (fromJSON, toJSON)
import Hedgehog
import Test.Tasty (TestTree (), testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Test.Elocrypt.Types"
    [testPropertyNamed "toJSON roundtrip" "prop_toJSON_roundtrip" prop_toJSON_roundtrip]

prop_toJSON_roundtrip :: Property
prop_toJSON_roundtrip = property $ do
  unigram <- forAll Gen.trigram
  tripping unigram toJSON fromJSON
