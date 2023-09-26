module Data.Gibberish.Format
  ( FormatOpts (..),
    MaxLen (..),
    MaxHeight (..),
    Separator (..),
    ExactNumberWords (..),
    Word (..),
    formatWords,
    formatLine,
  ) where

import Data.List (intersperse)
import Data.Text (Text ())
import Data.Text qualified as Text
import Prelude hiding (Word ())

data FormatOpts = FormatOpts
  { optMaxLen :: MaxLen,
    optMaxHeight :: MaxHeight,
    optSeparator :: Separator,
    optExactWords :: Maybe ExactNumberWords
  }
  deriving stock (Eq, Show)

newtype MaxLen = MaxLen {unMaxLen :: Int}
  deriving stock (Eq, Show)
  deriving newtype (Enum, Integral, Num, Ord, Real)

newtype MaxHeight = MaxHeight {unMaxHeight :: Int}
  deriving stock (Eq, Show)
  deriving newtype (Enum, Integral, Num, Ord, Real)

newtype Separator = Separator {unSeparator :: Text}
  deriving stock (Eq, Show)

newtype ExactNumberWords = ExactNumberWords {unExactWords :: Int}
  deriving stock (Eq, Show)
  deriving newtype (Enum, Integral, Num, Ord, Real)

newtype Word = Word {unWord :: Text}
  deriving stock (Eq, Show)

newtype FormatText = FormatText [FormatLine]
data FormatLine = FormatLine Separator [Word]

-- | Format a list of words to a text blob
formatWords :: FormatOpts -> [Word] -> Text
formatWords opts words' = renderFormatText (formatWords' opts words')

formatWords' :: FormatOpts -> [Word] -> FormatText
formatWords' opts@FormatOpts {..} words' =
  case optMaxHeight of
    1 -> FormatText [line]
    _ ->
      let (FormatText t) = formatWords' (opts {optMaxHeight = optMaxHeight - 1}) words'
      in FormatText $ line : t
  where
    line = formatLine opts words'

formatLine :: FormatOpts -> [Word] -> FormatLine
formatLine FormatOpts {..} =
  FormatLine optSeparator
    . map Word
    . filter (/= unSeparator optSeparator)
    . concatLine (unMaxLen optMaxLen)
    . intersperse (unSeparator optSeparator)
    . map unWord
  where
    concatLine :: Int -> [Text] -> [Text]
    concatLine len (t : ts)
      | len - Text.length t > 0 = t : concatLine (len - Text.length t) ts
      | otherwise = []
    concatLine _ [] = error "Ran out of words"

renderFormatText :: FormatText -> Text
renderFormatText (FormatText (l : ls)) =
  renderFormatLine l <> "\n" <> renderFormatText (FormatText ls)
  where
    renderFormatLine :: FormatLine -> Text
    renderFormatLine (FormatLine (Separator sep) (ws)) =
      Text.concat $ intersperse sep (map unWord ws)
renderFormatText (FormatText []) = ""
