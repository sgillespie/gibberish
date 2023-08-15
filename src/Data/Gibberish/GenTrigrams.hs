{-# LANGUAGE OverloadedLists #-}

module Data.Gibberish.GenTrigrams (mapTrigrams) where

import Data.Gibberish.Types
import Data.Map (Map ())
import Data.Map qualified as Map
import Data.Text (Text ())
import Data.Text qualified as Text

mapTrigrams :: [Text] -> Map Digram Frequencies
mapTrigrams [] = Map.empty
mapTrigrams (x : xs) = Map.unionWith combine (mkTrigrams x) (mapTrigrams xs)
  where
    combine (Frequencies f1) (Frequencies f2) = Frequencies $ Map.unionWith (+) f1 f2

mkTrigrams :: Text -> Map Digram Frequencies
mkTrigrams word = foldr insert' Map.empty $ scanTrigrams word
  where
    insert' (a, b, c) map' =
      Map.insertWith combineFrequencies (Digram a b) (mkFrequencies c) map'
    combineFrequencies (Frequencies m1) (Frequencies m2) = Frequencies (Map.unionWith (+) m1 m2)
    mkFrequencies c = Frequencies $ Map.singleton (Unigram c) 1

scanTrigrams :: Text -> [(Char, Char, Char)]
scanTrigrams word = case Text.take 3 word of
  [a, b, c] -> (a, b, c) : scanTrigrams (Text.tail word)
  _ -> []
