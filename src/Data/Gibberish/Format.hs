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

newtype FormatText = FormatText {fmtLines :: [FormatLine]}
  deriving stock (Eq, Show)

data FormatLine = FormatLine
  { fmtSeparator :: Separator,
    fmtWords :: [Word]
  }
  deriving stock (Eq, Show)

-- | Format a list of words to a text blob
formatWords :: FormatOpts -> [Word] -> Text
formatWords opts@FormatOpts {..} =
  renderFormatText . take' . formatWords' opts
  where
    take' :: FormatText -> FormatText
    take' =
      case optExactWords of
        Just (ExactNumberWords exact) -> takeWords exact
        Nothing -> takeLines (unMaxHeight optMaxHeight)

-- | Turn a list of words into a Format description. Note that we completely
-- ignore maxHeight and exactWords, resulting in a potentially infinite list
formatWords' :: FormatOpts -> [Word] -> FormatText
formatWords' opts words' =
  FormatText $
    line : fmtLines (formatWords' opts words')
  where
    line = formatLine opts words'

-- | Format a single line of words, up to maxLen characters
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

-- | Render a Format description into a Text blob
renderFormatText :: FormatText -> Text
renderFormatText (FormatText fmt) =
  case fmt of
    [] -> ""
    l : ls -> renderFormatLine l <> "\n" <> renderFormatText' ls
  where
    renderFormatLine (FormatLine (Separator sep) ws) =
      Text.concat . intersperse sep . map unWord $ ws
    renderFormatText' ls = renderFormatText (FormatText ls)

takeLines :: Int -> FormatText -> FormatText
takeLines n (FormatText ls) = FormatText $ take n ls

takeWords :: Int -> FormatText -> FormatText
takeWords _ (FormatText []) = error "Ran out of words"
takeWords n (FormatText (l@(FormatLine sep ws) : ls))
  | n >= length ws = FormatText $ l : ls'
  | otherwise = FormatText [FormatLine sep (take n ws)]
  where
    (FormatText ls') = takeWords (n - length ws) (FormatText ls)
