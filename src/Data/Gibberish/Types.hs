{-# LANGUAGE OverloadedLists #-}

module Data.Gibberish.Types
  ( GenPassOptions (..),
    Error (..),
    Unigram (..),
    Digram (..),
    Trigram (..),
    Frequency (..),
    Frequencies (..),
    Trigraph (..),
    Word (..),
  ) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception ())
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (FromJSONKeyFunction (..), Parser (), toJSONKeyText)
import Data.Map (Map ())
import Data.Text (Text ())
import Data.Typeable (Typeable ())
import GHC.Generics (Generic ())
import TextShow (TextShow (..), fromString)
import Prelude hiding (Word ())

-- | Password/Passphrase generation options
data GenPassOptions = GenPassOptions
  { -- | Include capitals?
    optsCapitals :: !Bool,
    -- | Include numerals?
    optsDigits :: !Bool,
    -- | Include special characters?
    optsSpecials :: !Bool,
    -- | The trigraph to use
    optsTrigraph :: Trigraph
  }
  deriving stock (Eq, Show)

-- | Exceptions that can occur at runtime
data Error
  = TrigraphNotFound FilePath
  | ImpossibleError
  deriving stock (Eq, Typeable)

-- | A unigram is a single letter
newtype Unigram = Unigram {unUnigram :: Char}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, FromJSONKey, NFData, ToJSON, ToJSONKey)

-- | A digram is a sequence of two letters
data Digram = Digram Char Char
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | A trigrams is a sequence of three letters
data Trigram = Trigram Char Char Char
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | A frequency represents the number of times a given trigram occurs
--   in a language
newtype Frequency = Frequency {unFequency :: Int}
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON, NFData, Enum, Integral, Num, Ord, Real)

-- | Frequencies maps a unigram to a frequency
newtype Frequencies = Frequencies {unFrequencies :: Map Unigram Frequency}
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON, NFData)

-- | A trigraph is a mapping of all digrams to frequencies. That is, for a set of
--   digrams, it contains the frequencies of all possible trigram candidates.
newtype Trigraph = Trigraph {unTrigraph :: Map Digram Frequencies}
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON, NFData)

-- | A natural language word
newtype Word = Word {unWord :: Text}
  deriving stock (Eq, Show)

instance Exception Error

instance Show Error where
  show (TrigraphNotFound path) = "Trigraph file " <> show path <> " does not exist!"
  show ImpossibleError = "The impossible happened! Please file a bug report."

instance TextShow Digram where
  showb (Digram c1 c2) = fromString [c1, c2]

instance ToJSON Digram where
  toJSON (Digram c1 c2) = Aeson.String [c1, c2]

instance ToJSONKey Digram where
  toJSONKey = toJSONKeyText toText
    where
      toText :: Digram -> Text
      toText = showt

instance FromJSON Digram where
  parseJSON = Aeson.withText "Digram" parseDigram

instance FromJSONKey Digram where
  fromJSONKey = FromJSONKeyTextParser parseDigram

instance Ord Digram where
  (Digram a1 b1) `compare` (Digram a2 b2) =
    (a1, b1) `compare` (a2, b2)

instance Ord Trigram where
  (Trigram a1 b1 c1) `compare` (Trigram a2 b2 c2) =
    (a1, b1, c1) `compare` (a2, b2, c2)

parseDigram :: Text -> Parser Digram
parseDigram = (uncurry Digram <$>) . fromText
  where
    fromText [ch1, ch2] = pure (ch1, ch2)
    fromText _ = fail "Not a string of length 2"
