module Data.Gibberish.GenPassSpec (spec) where

import Data.Gibberish.GenPass (genPassword)
import Data.Gibberish.MonadPass (usingPass)
import Data.Gibberish.Trigraph (Language (..), loadTrigraph)
import Data.Gibberish.Types (GenPassOptions (..), Word (..))
import Test.Gibberish.Gen qualified as Gen

import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as Text
import Hedgehog
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
