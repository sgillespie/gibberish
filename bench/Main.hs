module Main (main) where

import Data.Gibberish.GenTrigraph (genTrigraph)
import Data.Gibberish.Types (Trigraph (..))
import Paths_gibberish (getDataDir)

import Criterion.Main
import Data.Aeson (encode)
import Data.Char (isAlpha)
import Data.Map (Map ())
import Data.Text (Text ())
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.FilePath ((</>))

dictionary :: IO [Text]
dictionary = map Text.toLower . filter (Text.all isAlpha) . Text.lines <$> contents
  where
    contents = Text.readFile =<< dataDir
    dataDir = (</> "data" </> "dicts" </> "wamerican.txt") <$> getDataDir

trigraph :: IO Trigraph
trigraph = genTrigraph <$> dictionary

main :: IO ()
main =
  defaultMain
    [ bench "load dictionary file" $ nfIO dictionary,
      env dictionary $ bench "generate trigraph" . nf genTrigraph,
      env trigraph $ bench "serialize to JSON" . nf encode
    ]
