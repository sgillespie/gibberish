module Data.Gibberish.GenPassSpec (spec) where

import Data.Gibberish.GenPass (genPassword)
import Data.Gibberish.MonadPass (usingPass)
import Data.Gibberish.Trigraph (Language (..), loadTrigraph)
import Data.Gibberish.Types (GenPassOptions (..), Word (..))
import Data.Gibberish.Utils (numeralConversions, symbolConversions)
import Test.Gibberish.Gen qualified as Gen

import Control.Monad.IO.Class (liftIO)
import Data.Char
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

    it "sometimes has multiple capitals when enabled" $ hedgehog $ do
      trigraph <- liftIO $ loadTrigraph English
      opts <- forAll Gen.genPassOptions
      randomGen <- forAll Gen.stdGen
      -- Only consider passwords of sufficient (>=10) length
      len <- forAll (Gen.int $ Range.linear 10 20)

      let opts' =
            opts
              { optsTrigraph = trigraph,
                optsCapitals = True,
                optsLength = len
              }

      let (Word pass, _) = usingPass randomGen (genPassword opts')
      annotateShow pass

      cover 10 "has multiple capitals" $
        Text.length (Text.filter isUpperCase pass) > 1

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
        Text.any isNumber pass
          || Text.all (`Map.notMember` numeralConversions) pass

    it "sometimes has multiple digits when enabled" $ hedgehog $ do
      trigraph <- liftIO $ loadTrigraph English
      opts <- forAll Gen.genPassOptions
      randomGen <- forAll Gen.stdGen
      -- Only consider passwords of sufficient (>=10) length
      len <- forAll (Gen.int $ Range.linear 10 20)

      let opts' =
            opts
              { optsTrigraph = trigraph,
                optsDigits = True,
                optsLength = len
              }

      let (Word pass, _) = usingPass randomGen (genPassword opts')
      annotateShow pass

      cover 10 "has multiple digits" $
        Text.length (Text.filter isNumber pass) > 1

    it "usually has at least one special when enabled" $ hedgehog $ do
      trigraph <- liftIO $ loadTrigraph English
      opts <- forAll Gen.genPassOptions
      randomGen <- forAll Gen.stdGen
      -- Only consider passwords of sufficient (>=3) length
      len <- forAll (Gen.int $ Range.linear 3 15)

      let opts' =
            opts
              { optsTrigraph = trigraph,
                optsSpecials = True,
                optsLength = len
              }

      let (Word pass, _) = usingPass randomGen (genPassword opts')
      annotateShow pass

      let allSymbols = concat (Map.elems symbolConversions)

      cover 50 "has at least one special" $
        Text.any (`elem` allSymbols) pass

  it "sometimes has at multiple specials when enabled" $ hedgehog $ do
    trigraph <- liftIO $ loadTrigraph English
    opts <- forAll Gen.genPassOptions
    randomGen <- forAll Gen.stdGen
    -- Only consider passwords of sufficient (>=10) length
    len <- forAll (Gen.int $ Range.linear 10 20)

    let opts' =
          opts
            { optsTrigraph = trigraph,
              optsSpecials = True,
              optsLength = len
            }

    let (Word pass, _) = usingPass randomGen (genPassword opts')
    annotateShow pass

    let allSymbols = concat (Map.elems symbolConversions)

    cover 10 "has at least one special" $
      Text.length (Text.filter (`elem` allSymbols) pass) > 1
