module Data.Gibberish.UtilsSpec (spec) where

import Data.Gibberish.MonadPass (usingPass)
import Data.Gibberish.Utils
import Test.Gibberish.Gen qualified as Gen

import Data.Char (isLowerCase, isUpperCase, toUpper)
import Data.Maybe (listToMaybe)
import Data.Ratio ((%))
import Data.Text qualified as Text
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = do
  describe "update1" $ do
    it "updates at index" $ hedgehog $ do
      t <- forAll $ Gen.text (Range.linear 1 250) Gen.lower
      ix <- forAll $ Gen.int (Range.linear 0 (Text.length t - 1))
      res <- update1 (pure . toUpper) t ix

      assert $ isUpperCase (Text.index res ix)

  describe "updateR" $ do
    it "updates everything when ratio is infinity" $ hedgehog $ do
      t <- forAll $ Gen.text (Range.linear 1 250) Gen.lower
      randomGen <- forAll Gen.stdGen

      -- Make numerator absurdly high since we can't use 0 in the denominator
      let prob = fromIntegral (maxBound :: Int) % 1
          (res, _) = usingPass randomGen (updateR (pure . toUpper) prob t)
      annotateShow res

      assert $ Text.all isUpperCase res

    it "updates nothing when ratio is zero" $ hedgehog $ do
      t <- forAll $ Gen.text (Range.linear 1 250) Gen.lower
      randomGen <- forAll Gen.stdGen

      let (res, _) = usingPass randomGen (updateR (pure . toUpper) (0 % 1) t)
      annotateShow res

      assert $ Text.all isLowerCase res

  describe "findIndices" $ do
    it "agrees with findIndex" $ hedgehog $ do
      t <- forAll $ Gen.text (Range.linear 1 250) Gen.digit
      let mapper = (<= '5')
      listToMaybe (findIndices mapper t) === Text.findIndex mapper t

    it "agrees with filter" $ hedgehog $ do
      t <- forAll $ Gen.text (Range.linear 1 250) Gen.digit
      let p = (<= '5')
      map (t `Text.index`) (findIndices p t) === Text.unpack (Text.filter p t)

  describe "textTraverse" $ do
    it "agrees with map" $ hedgehog $ do
      t <- forAll $ Gen.text (Range.linear 1 250) Gen.lower
      let mapper = pure . toUpper
      res <- textTraverse mapper t

      res === Text.map toUpper t
