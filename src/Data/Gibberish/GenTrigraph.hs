{-# LANGUAGE OverloadedLists #-}

module Data.Gibberish.GenTrigraph (genTrigraph) where

import Data.Gibberish.Types
import Data.Map.Strict (Map ())
import Data.Map.Strict qualified as Map
import Data.Text (Text ())
import Data.Text qualified as Text

-- | Generate trigraphs from a list of words
genTrigraph :: [Text] -> Trigraph
genTrigraph = Trigraph . foldr foldWord Map.empty
  where
    foldWord = Map.unionWith combine . mkTrigraph
    combine (Frequencies f1) (Frequencies f2) = Frequencies $ Map.unionWith (+) f1 f2

-- | Generate a trigraph from a single word
mkTrigraph :: Text -> Map Digram Frequencies
mkTrigraph word = foldr insert' Map.empty $ scanTrigrams word
  where
    insert' (Trigram a b c) =
      Map.insertWith combineFrequencies (Digram a b) (mkFrequencies c)
    combineFrequencies (Frequencies m1) (Frequencies m2) = Frequencies (Map.unionWith (+) m1 m2)
    mkFrequencies c = Frequencies $ Map.singleton (Unigram c) 1

scanTrigrams :: Text -> [Trigram]
scanTrigrams word = case Text.take 3 word of
  [a, b, c] -> Trigram a b c : scanTrigrams (Text.tail word)
  _ -> []
