module Test.Gibberish.TypesTest (tests) where

import Test.Gibberish.Gen qualified as Gen

import Data.Aeson (fromJSON, toJSON)
import Hedgehog
import Test.Tasty (TestTree (), testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Test.Gibberish.Types"
    [testPropertyNamed "toJSON roundtrip" "prop_toJSON_roundtrip" prop_toJSON_roundtrip]

prop_toJSON_roundtrip :: Property
prop_toJSON_roundtrip = property $ do
  trigraph <- forAll Gen.trigraph
  tripping trigraph toJSON fromJSON
