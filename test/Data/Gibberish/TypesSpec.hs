module Data.Gibberish.TypesSpec (spec) where

import Test.Gibberish.Gen qualified as Gen

import Data.Aeson (fromJSON, toJSON)
import Hedgehog
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = do
  describe "toJSON" $ do
    it "roundtrip" $ hedgehog $ do
      trigraph <- forAll Gen.trigraph
      tripping trigraph toJSON fromJSON
