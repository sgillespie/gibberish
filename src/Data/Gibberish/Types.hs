{-# LANGUAGE OverloadedLists #-}

module Data.Gibberish.Types
  ( Unigram (..),
    Digram (..),
    Frequency (..),
    Frequencies (..),
    Trigram (..),
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (FromJSONKeyFunction (..), Parser (), toJSONKeyText)
import Data.Map (Map ())
import Data.Text (Text ())
import GHC.Generics (Generic ())
import TextShow (TextShow (..), fromString)

newtype Unigram = Unigram {unUnigram :: Char}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, FromJSONKey, NFData, ToJSON, ToJSONKey)

data Digram = Digram Char Char
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

newtype Frequency = Frequency {unFequency :: Int}
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON, NFData, Enum, Integral, Num, Ord, Real)

newtype Frequencies = Frequencies {unFrequencies :: Map Unigram Frequency}
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON, NFData)

newtype Trigram = Trigram {unTrigram :: Map Digram Frequencies}
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON, NFData)

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
    case a1 `compare` a2 of
      EQ -> b1 `compare` b2
      c -> c

parseDigram :: Text -> Parser Digram
parseDigram = (uncurry Digram <$>) . fromText
  where
    fromText [ch1, ch2] = pure (ch1, ch2)
    fromText _ = fail "Not a string of length 2"