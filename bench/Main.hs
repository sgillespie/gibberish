module Main (main) where

import Data.Gibberish (Trigraph (..), Word (..), genTrigraph)
import Data.Gibberish.Formatting qualified as Fmt
import Paths_gibberish (getDataDir)

import Criterion.Main
import Data.Aeson (encode)
import Data.Char (isAlpha)
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
    [ -- Trigraph generation
      bench "load dictionary file" $ nfIO dictionary,
      env dictionary $ bench "generate trigraph" . nf genTrigraph,
      env trigraph $ bench "serialize to JSON" . nf encode,
      -- Formatting
      env dictionary $ bench "format words" . nf formatWords'
    ]

formatWords' :: [Text] -> Text
formatWords' = Fmt.formatWords opts . map Word
  where
    opts =
      Fmt.FormatOpts
        { optMaxLen = Fmt.MaxLen 100,
          optMaxHeight = Fmt.MaxHeight 1000,
          optSeparator = Fmt.Separator " ",
          optExactWords = Nothing
        }
