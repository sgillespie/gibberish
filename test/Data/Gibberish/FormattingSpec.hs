module Data.Gibberish.FormattingSpec (spec) where

import Data.Gibberish.Formatting
import Data.Gibberish.Types

import Control.Monad (forM_)
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
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
      it "words (formatWords w) `isPrefixOf` w" $ hedgehog $ do
        opts@FormatOpts {..} <- forAll genFormatOpts
        words' <-
          forAll $
            genWords
              (Range.linear 3 10)
              (fromIntegral optMaxLen * fromIntegral optMaxHeight)

        let formatted = formatWords opts words'
            res = map Word (Text.words formatted)

        annotateShow res

        assert $ res `isPrefixOf` words'

      it "minLen <= length formatWords <= maxLen" $ hedgehog $ do
        opts@FormatOpts {..} <- forAll genFormatOpts
        lines' <- forAll $ genFormattedLines (Range.linear 3 10) opts

        annotateLines lines'

        forM_ lines' $ \line ->
          assert $
            Text.length line <= fromIntegral optMaxLen
              && Text.length line >= minLen 10 optSeparator optMaxLen

      it "length (lines (formatWords)) == maxHeight" $ hedgehog $ do
        opts@FormatOpts {..} <- forAll genFormatOpts
        lines' <- forAll $ genFormattedLines (Range.linear 3 10) opts

        length lines' === fromIntegral optMaxHeight

      it "prints exact" $ hedgehog $ do
        exact <- forAll genExactWords
        opts <- forAll $ genFormatOpts' (pure $ Just exact)
        lines' <- forAll $ genFormattedLines (Range.linear 3 10) opts

        let words' = Text.words (Text.unlines lines')
        length words' === unExactWords exact

    describe "constant length" $ do
      it "words (formatWords w) `isPrefixOf` w" $ hedgehog $ do
        wordLen <- forAll $ Gen.int (Range.linear 3 10)
        opts@FormatOpts {..} <- forAll genFormatOpts
        words' <-
          forAll $
            genWords
              (Range.singleton wordLen)
              (fromIntegral optMaxLen * fromIntegral optMaxHeight)

        let formatted = formatWords opts words'
            res = map Word (Text.words formatted)

        annotateShow res

        assert $ res `isPrefixOf` words'

      it "minLen <= length formatWords <= maxLen" $ hedgehog $ do
        wordLen <- forAll $ Gen.int (Range.linear 3 10)
        opts@FormatOpts {..} <- forAll genFormatOpts
        lines' <- forAll $ genFormattedLines (Range.singleton wordLen) opts

        annotateLines lines'

        forM_ lines' $ \line ->
          assert $
            Text.length line <= fromIntegral optMaxLen
              && Text.length line >= minLen wordLen optSeparator optMaxLen

      it "length (lines (formatWords)) == maxHeight" $ hedgehog $ do
        wordLen <- forAll $ Gen.int (Range.linear 3 10)
        opts@FormatOpts {..} <- forAll genFormatOpts
        lines' <- forAll $ genFormattedLines (Range.singleton wordLen) opts

        length lines' === fromIntegral optMaxHeight

genFormatOpts :: Gen FormatOpts
genFormatOpts = genFormatOpts' $ pure Nothing

genFormatOpts' :: Gen (Maybe ExactNumberWords) -> Gen FormatOpts
genFormatOpts' exact =
  FormatOpts
    <$> genMaxLen
    <*> genMaxHeight
    <*> genSeparator
    <*> exact

genMaxLen :: Gen MaxLen
genMaxLen = MaxLen <$> Gen.integral (Range.linear 50 100)

genMaxHeight :: Gen MaxHeight
genMaxHeight = MaxHeight <$> Gen.integral (Range.linear 3 50)

genSeparator :: Gen Separator
genSeparator = Separator <$> Gen.text (Range.linear 1 3) (pure ' ')

genExactWords :: Gen ExactNumberWords
genExactWords = ExactNumberWords <$> Gen.integral (Range.linear 0 100)

minLen :: Int -> Separator -> MaxLen -> Int
minLen maxWordSize (Separator sep) (MaxLen maxLen) =
  maxLen - (maxWordSize + Text.length sep)

genFormattedLines
  :: Range Int
  -> FormatOpts
  -> Gen [Text]
genFormattedLines wordLen opts@FormatOpts {..} = do
  words' <- genWords wordLen (fromIntegral optMaxLen * fromIntegral optMaxHeight)
  pure $ Text.lines (formatWords opts words')

genWords :: Range Int -> Int -> Gen [Word]
genWords wordLen maxLen = Gen.list (Range.singleton minWords) (genWord wordLen)
  where
    minWords = maxLen `div` 2

genWord :: Range Int -> Gen Word
genWord len = Word <$> Gen.text len (Gen.enum 'a' 'e')

annotateLines :: (MonadTest m, HasCallStack) => [Text] -> m ()
annotateLines lines' =
  annotateShow $
    lines' <&> \line ->
      line <> "(length=" <> showt (Text.length line) <> ")"
