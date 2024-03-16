{-# LANGUAGE OverloadedLists #-}

module Data.Gibberish.Trigraph
  ( Language (..),
    TrigraphConfig (..),
    genTrigraph,
    loadTrigraph,
  ) where

import Data.Gibberish.Utils (toQwertyKey)
import Paths_gibberish (getDataFileName)

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Aeson qualified as Aeson
import Data.Char (isPunctuation, toLower)
import Data.Gibberish.Types
import Data.Map.Strict (Map ())
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text ())
import Data.Text qualified as Text
import System.Directory (doesFileExist)
import System.FilePath ((</>))

data Language
  = English
  | Spanish
  | CustomTrigraph TrigraphConfig
  deriving stock (Eq, Show)

newtype TrigraphConfig = TrigraphConfig
  {unTrigraphConfig :: FilePath}
  deriving stock (Eq, Show)

-- | Generate trigraphs from a list of words
genTrigraph :: [Text] -> Trigraph
genTrigraph = Trigraph . foldr (foldWord . normalizeWord) Map.empty
  where
    foldWord = Map.unionWith combine . mkTrigraph
    combine (Frequencies f1) (Frequencies f2) = Frequencies $ Map.unionWith (+) f1 f2

-- | Generate a trigraph from a single word
mkTrigraph :: Text -> Map Digram Frequencies
mkTrigraph word = foldr insert' Map.empty $ scanTrigrams word
  where
    insert' (Trigram a b c) =
      Map.insertWith combineFrequencies (Digram a b) (mkFrequencies c)
    combineFrequencies (Frequencies m1) (Frequencies m2) =
      Frequencies (Map.unionWith (+) m1 m2)
    mkFrequencies c = Frequencies $ Map.singleton (Unigram c) 1

-- | Normalize a word before calculating the trigraph:
--
--  1. Remove punctuation (quotes, dashes, and so on)
--  2. Lower case all letters
--  3. Translate non-qwerty chars to qwerty keys (eg, à -> a)
normalizeWord :: Text -> Text
normalizeWord = Text.map transformChar . Text.filter filterChar
  where
    transformChar :: Char -> Char
    transformChar = toQwertyKey . toLower

    filterChar :: Char -> Bool
    filterChar = not . isPunctuation

scanTrigrams :: Text -> [Trigram]
scanTrigrams word = case Text.take 3 word of
  [a, b, c] -> Trigram a b c : scanTrigrams (Text.tail word)
  _ -> []

loadTrigraph :: Language -> IO Trigraph
loadTrigraph English = loadBuiltinTrigraph "wamerican.json"
loadTrigraph Spanish = loadBuiltinTrigraph "wspanish.json"
loadTrigraph (CustomTrigraph cfg) = loadTrigraphFromFile (unTrigraphConfig cfg)

loadBuiltinTrigraph :: FilePath -> IO Trigraph
loadBuiltinTrigraph file' = loadTrigraphFromFile =<< getBuiltinFilePath file'
  where
    getBuiltinFilePath basename = getDataFileName ("data" </> "trigraphs" </> basename)

loadTrigraphFromFile :: FilePath -> IO Trigraph
loadTrigraphFromFile file' = do
  exists <- doesFileExist file'
  unless exists $
    throwIO (TrigraphNotFound file')

  fromJust <$> Aeson.decodeFileStrict file'
