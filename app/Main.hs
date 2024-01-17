module Main (main) where

import Data.Elocrypt
import Data.Gibberish.Format qualified as Fmt
import Data.Gibberish.Types (Word (..))

import Data.Maybe (fromMaybe)
import Data.Text (Text ())
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Version (showVersion)
import Options.Applicative hiding (columns)
import Options.Applicative.Help.Pretty
import PackageInfo_gibberish (name, version)
import System.Environment (getArgs)
import System.Random (RandomGen (..), getStdGen)
import Prelude hiding (Word ())

termLen :: Int
termLen = 80

termHeight :: Int
termHeight = 10

data Options = Options
  { -- | Options that are always used
    optCommon :: CommonOpts,
    -- | Options that only apply to a specific types
    optType :: Either WordOpts PhraseOpts
  }
  deriving stock (Eq, Show)

data CommonOpts = CommonOpts
  { -- | Include capitals?
    optCapitals :: !Bool,
    -- | Include numerals?
    optDigits :: !Bool,
    -- | Include special characters?
    optSpecials :: !Bool,
    -- | How many passwords/phrases to generate
    optNumber :: !(Maybe Int)
  }
  deriving stock (Eq, Show)

data WordOpts = WordOpts
  {optLength :: Int}
  deriving stock (Eq, Show)

data PhraseOpts = PhraseOpts
  { optMinLength :: Int,
    optMaxLength :: Int
  }
  deriving stock (Eq, Show)

data PassType = Passphrase | Password
  deriving stock (Eq, Show)

main :: IO ()
main = run =<< execParser' opts
  where
    opts = info (optsParser <**> parseVersion <**> helper) mods
    mods = briefDesc <> progDesc desc
    desc =
      "Generates pronounceable passwords that are easy-to-remember and\
      \ hard-to-guess."

run :: Options -> IO ()
run (Options {..}) = Text.putStrLn . run' optType =<< getStdGen
  where
    run' (Left w) = passwords optCommon w
    run' (Right p) = passphrases optCommon p

passwords :: RandomGen gen => CommonOpts -> WordOpts -> gen -> Text
passwords opts@(CommonOpts {..}) (WordOpts {..}) gen =
  Fmt.formatWords formatOpts (map (Word . Text.pack) passwords')
  where
    passwords' = newPasswords optLength num (getGenOptions opts) gen
    num = fromMaybe (numFitWords + 1) optNumber
    numFitWords = wordsPerLine * termHeight
    wordsPerLine = termLen + Text.length sep' `div` optLength + Text.length sep'
    sep' = "  "
    formatOpts =
      Fmt.FormatOpts
        { optMaxLen = Fmt.MaxLen termLen,
          optMaxHeight = Fmt.MaxHeight termHeight,
          optSeparator = Fmt.Separator "  ",
          optExactWords = Fmt.ExactNumberWords <$> optNumber
        }

passphrases :: RandomGen gen => CommonOpts -> PhraseOpts -> gen -> Text
passphrases opts@(CommonOpts {..}) (PhraseOpts {..}) gen =
  Text.intercalate " " $ map Text.pack passphrases'
  where
    passphrases' = newPassphrase num optMinLength optMaxLength (getGenOptions opts) gen
    num = fromMaybe termLen optNumber

getGenOptions :: CommonOpts -> GenOptions
getGenOptions CommonOpts {..} =
  genOptions
    { genCapitals = optCapitals,
      genDigits = optDigits,
      genSpecials = optSpecials
    }

execParser' :: ParserInfo a -> IO a
execParser' info' =
  execParserPure defaultPrefs info' <$> getArgs
    >>= handleParseResult . overFailure'

overFailure' :: ParserResult a -> ParserResult a
overFailure' = overFailure $ \help' -> help' {helpUsage = pure usage}

usage :: Doc
usage =
  hsep
    [ pretty ("Usage:" :: Text),
      align $
        vsep
          [ "gibber [option...] length",
            "gibber --passphrase [option...] min-length max-length"
          ]
    ]

optsParser :: Parser Options
optsParser = parseOptions
  where
    parseOptions =
      Options
        <$> parseCommonOpts
        <*> parseTypeOpts

parseCommonOpts :: Parser CommonOpts
parseCommonOpts =
  CommonOpts
    <$> parseCapitals
    <*> parseDigits
    <*> parseSpecials
    <*> parseNumber

parseCapitals :: Parser Bool
parseCapitals =
  switch $
    long "capitals"
      <> short 'c'
      <> help "Include at least one capital letter"

parseDigits :: Parser Bool
parseDigits =
  switch $
    long "digits"
      <> short 'd'
      <> help "Include numerals"

parseSpecials :: Parser Bool
parseSpecials =
  switch $
    long "symbols"
      <> short 's'
      <> help "Include special characters"

parseNumber :: Parser (Maybe Int)
parseNumber =
  optional $
    option auto $
      long "number"
        <> short 'n'
        <> metavar "NUMBER"
        <> help "The number of passwords to generate"

parseTypeOpts :: Parser (Either WordOpts PhraseOpts)
parseTypeOpts =
  (Left <$> parseWordOpts) <|> (Right <$> parsePhraseOpts)

parseWordOpts :: Parser WordOpts
parseWordOpts =
  (WordOpts <$> parseLength)

parsePhraseOpts :: Parser PhraseOpts
parsePhraseOpts =
  parseTypePhrase *> (PhraseOpts <$> parseMinLength <*> parseMaxLength)

parseTypePhrase :: Parser PassType
parseTypePhrase =
  flag' Passphrase $
    long "passphrase"
      <> short 'p'
      <> help "Generate passphrases instead of passwords"

parseVersion :: Parser (a -> a)
parseVersion =
  infoOption (Text.unpack showVersion') $
    long "version"
      <> short 'v'
      <> help "Show version information"

parseLength :: Parser Int
parseLength = argument auto (metavar "length")

parseMinLength :: Parser Int
parseMinLength = argument auto (metavar "min-length")

parseMaxLength :: Parser Int
parseMaxLength =
  argument auto $
    metavar "max-length"
      <> value 10

showVersion' :: Text
showVersion' = name' <> " version " <> version'
  where
    version' = Text.pack (showVersion version)
    name' = Text.toTitle (Text.pack name)
