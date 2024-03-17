module Main (main) where

import Data.Gibberish (genTrigraph)

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (ByteString ())
import Data.ByteString.Lazy qualified as ByteString
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Options.Applicative
import Prelude hiding (writeFile)

data Options = Options
  { optInputFile :: FilePath,
    optOutput :: OptOutput
  }
  deriving stock (Eq, Show)

data OptOutput = StdOut | OutputFile FilePath
  deriving stock (Eq, Show)

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (optsParser <**> helper) mods
    mods = fullDesc <> progDesc "Generates a JSON trigraph from a dictionary"

run :: Options -> IO ()
run Options {..} = writeOutput' . genTrigraph' =<< readInputFile
  where
    genTrigraph' = encodePretty . genTrigraph . Text.lines
    writeOutput' = writeOutput optOutput
    readInputFile = Text.readFile optInputFile

optsParser :: Parser Options
optsParser =
  Options
    <$> strArgument inputFile
    <*> parseOptOutput
  where
    inputFile =
      metavar "<file>"
        <> help "The dictionary file to analyze"

parseOptOutput :: Parser OptOutput
parseOptOutput =
  (OutputFile <$> outFile) <|> stdOut
  where
    stdOut =
      flag StdOut StdOut $
        long "stdout"
          <> short 'o'
          <> help "Write JSON output to <file>"

    outFile =
      strOption $
        long "out-file"
          <> short 'f'
          <> help "Write JSON output to stdout"

writeOutput :: OptOutput -> ByteString -> IO ()
writeOutput (OutputFile out) = ByteString.writeFile out
writeOutput StdOut = ByteString.putStr
