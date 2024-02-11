module Data.Gibberish.GenPassSpec (spec) where

import Data.Gibberish.GenPass (genPassword, numeralConversions)
import Data.Gibberish.MonadPass (usingPass)
import Data.Gibberish.Trigraph (Language (..), loadTrigraph)
import Data.Gibberish.Types (GenPassOptions (..), Word (..))
import Test.Gibberish.Gen qualified as Gen

import Control.Monad.IO.Class (liftIO)
import Data.Char
import Data.Map (Map ())
import Data.Map qualified as Map
import Data.Text qualified as Text
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog
import Prelude hiding (Word)

spec :: Spec
spec = do
  describe "genPassword" $ do
    it "has correct length" $ hedgehog $ do
      trigraph <- liftIO $ loadTrigraph English
      opts <- forAll Gen.genPassOptions
      randomGen <- forAll Gen.stdGen

      let opts' = opts {optsTrigraph = trigraph}

      let (Word pass, _) = usingPass randomGen (genPassword opts')
      annotateShow pass

      Text.length pass === optsLength opts

    it "has only lowercase when capitals is false" $ hedgehog $ do
      trigraph <- liftIO $ loadTrigraph English
      opts <- forAll Gen.genPassOptions
      randomGen <- forAll Gen.stdGen
      -- Only consider passwords of sufficient (>=3) length
      len <- forAll (Gen.int $ Range.linear 3 15)

      let opts' =
            opts
              { optsTrigraph = trigraph,
                optsCapitals = False,
                optsLength = len
              }

      let (Word pass, _) = usingPass randomGen (genPassword opts')
      annotateShow pass

      assert $ Text.all (not . isUpperCase) pass

    it "has at least one capital when enabled" $ hedgehog $ do
      trigraph <- liftIO $ loadTrigraph English
      opts <- forAll Gen.genPassOptions
      randomGen <- forAll Gen.stdGen
      -- Only consider passwords of sufficient (>=3) length
      len <- forAll (Gen.int $ Range.linear 3 15)

      let opts' =
            opts
              { optsTrigraph = trigraph,
                optsCapitals = True,
                optsLength = len
              }

      let (Word pass, _) = usingPass randomGen (genPassword opts')
      annotateShow pass

      assert $ Text.any (\c -> isUpperCase c || isPunctuation c) pass

    it "has at least one digit when enabled" $ hedgehog $ do
      trigraph <- liftIO $ loadTrigraph English
      opts <- forAll Gen.genPassOptions
      randomGen <- forAll Gen.stdGen
      -- Only consider passwords of sufficient (>=3) length
      len <- forAll (Gen.int $ Range.linear 3 15)

      let opts' =
            opts
              { optsTrigraph = trigraph,
                optsDigits = True,
                optsLength = len
              }

      let (Word pass, _) = usingPass randomGen (genPassword opts')
      annotateShow pass

      assert $
        Text.any (\c -> isNumber c) pass
          || Text.all (`Map.notMember` numeralConversions) pass
