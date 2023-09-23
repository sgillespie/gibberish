module Data.Gibberish.Format
  ( MaxLen (..),
    MaxHeight (..),
    Separator (..),
    Word (..),
    formatWords,
    formatLine,
  ) where

import Data.List (intersperse)
import Data.Text (Text ())
import Data.Text qualified as Text
import Prelude hiding (Word ())

newtype MaxLen = MaxLen {unMaxLen :: Int}
  deriving stock (Eq, Show)
  deriving newtype (Enum, Integral, Num, Ord, Real)

newtype MaxHeight = MaxHeight {unMaxHeight :: Int}
  deriving stock (Eq, Show)
  deriving newtype (Enum, Integral, Num, Ord, Real)

newtype Separator = Separator {unSeparator :: Text}
  deriving stock (Eq, Show)

newtype Word = Word {unWord :: Text}
  deriving stock (Eq, Show)

-- | Format a list of words to a text blob
formatWords :: MaxLen -> MaxHeight -> Separator -> [Word] -> Text
formatWords maxLen height sep words' =
  case height of
    1 -> line
    _ -> line <> "\n" <> formatWords maxLen (height - 1) sep words'
  where
    line = formatLine maxLen sep words'

formatLine :: MaxLen -> Separator -> [Word] -> Text
formatLine (MaxLen maxLen) (Separator sep) =
  concatLine maxLen . intersperse sep . map unWord
  where
    concatLine :: Int -> [Text] -> Text
    concatLine len (t : ts)
      | len - Text.length t > 0 = t <> concatLine (len - Text.length t) ts
      | otherwise = ""
    concatLine _ [] = error "Ran out of words"
