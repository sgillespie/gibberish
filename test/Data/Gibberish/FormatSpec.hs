module Data.Gibberish.FormatSpec (spec) where

import Data.Gibberish.Format

import Control.Monad (forM_)
import Data.Functor ((<&>))
import Data.Text (Text ())
import Data.Text qualified as Text
import Hedgehog
import Hedgehog.Gen qualified as Gen hiding (word)
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog
import TextShow (TextShow (..))
import Prelude hiding (Word)

spec :: Spec
spec = do
  describe "formatWords" $ do
    describe "variable length" $ do
      it "minLen <= length formatWords <= maxLen" $ hedgehog $ do
        maxLen <- forAll genMaxLen
        maxHeight <- forAll genMaxHeight
        output <-
          forAll $
            genFormattedLine (Range.linear 3 10) maxLen maxHeight
        let lines' = Text.lines output

        annotateShow $
          lines' <&> \line ->
            line <> "(length=" <> showt (Text.length line) <> ")"

        let minLen = maxLen - 10
        forM_ lines' $ \line ->
          assert $
            Text.length line <= fromIntegral maxLen
              && Text.length line >= fromIntegral minLen

      it "length (lines (formatWords)) == maxHeight" $ hedgehog $ do
        maxLen <- forAll genMaxLen
        maxHeight <- forAll genMaxHeight
        output <-
          forAll $
            genFormattedLine (Range.linear 3 10) maxLen maxHeight
        let lines' = Text.lines output

        length lines' === fromIntegral maxHeight

    describe "constant length" $ do
      it "minLen <= length formatWords <= maxLen" $ hedgehog $ do
        maxLen <- forAll genMaxLen
        maxHeight <- forAll genMaxHeight
        wordLen <- forAll $ Gen.int (Range.linear 3 10)
        output <-
          forAll $
            genFormattedLine (Range.singleton wordLen) maxLen maxHeight
        let lines' = Text.lines output

        annotateShow $
          lines' <&> \line ->
            line <> "(length=" <> showt (Text.length line) <> ")"

        let minLen = maxLen - 10
        forM_ lines' $ \line ->
          assert $
            Text.length line <= fromIntegral maxLen
              && Text.length line >= fromIntegral minLen

    it "length (lines (formatWords)) == maxHeight" $ hedgehog $ do
      maxLen <- forAll genMaxLen
      maxHeight <- forAll genMaxHeight
      wordLen <- forAll $ Gen.int (Range.linear 3 10)
      output <-
        forAll $
          genFormattedLine (Range.singleton wordLen) maxLen maxHeight
      let lines' = Text.lines output

      length lines' === fromIntegral maxHeight

genFormattedLine :: Range Int -> MaxLen -> MaxHeight -> Gen Text
genFormattedLine wordLen lineLen lineHeight = do
  separator <- genSeparator
  words' <- genWords wordLen (fromIntegral lineLen * fromIntegral lineHeight)

  let opts =
        FormatOpts
          { optMaxLen = lineLen,
            optMaxHeight = lineHeight,
            optSeparator = separator,
            optExactWords = Nothing
          }

  pure $ formatWords opts words'

genWords :: Range Int -> Int -> Gen [Word]
genWords wordLen maxLen = Gen.list (Range.singleton minWords) (genWord wordLen)
  where
    minWords = maxLen `div` 2

genWord :: Range Int -> Gen Word
genWord len = Word <$> Gen.text len (Gen.enum 'a' 'e')

genMaxLen :: Gen MaxLen
genMaxLen = MaxLen <$> Gen.integral (Range.linear 50 100)

genMaxHeight :: Gen MaxHeight
genMaxHeight = MaxHeight <$> Gen.integral (Range.linear 3 50)

genSeparator :: Gen Separator
genSeparator = Separator <$> Gen.text (Range.linear 1 3) (pure ' ')
