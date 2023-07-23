-- | A re-export of QuickCheck along with some useful instances
module Test.Elocrypt.QuickCheck
  ( module Test.QuickCheck,
    CliOptions (..),
    LessThan20 (..),
    LessThan40 (..),
    LessThan79 (..),
    PhraseCliOptions (..),
    WordCliOptions (..),
    getOptions,
    readHandle,
    run,
  ) where

import Data.Bool
import Data.List
import Data.Maybe
import System.Random
import Test.Proctest hiding (run)
import qualified Test.Proctest as Proctest
import Test.Proctest.Assertions
import Test.QuickCheck

import Data.Elocrypt

instance Arbitrary GenOptions where
  arbitrary = do
    caps <- arbitrary
    return genOptions {genCapitals = caps}

-- | A representation of elocrypt's command line
--  options
data CliOptions = CliOptions
  { cliCapitals :: Bool,
    cliDigits :: Bool,
    cliLength :: Maybe Int,
    cliMaxLength :: Maybe Int,
    cliNumber :: Maybe Int,
    cliPassphrase :: Bool,
    cliSpecials :: Bool
  }
  deriving (Eq)

instance Show CliOptions where
  show = unwords . getOptions

-- | CliOptions for generating only passwords
newtype WordCliOptions = WordCliOptions {getWordOpts :: CliOptions}
  deriving (Eq)

instance Show WordCliOptions where
  show = show . getWordOpts

instance Arbitrary WordCliOptions where
  arbitrary = do
    caps <- arbitrary
    digits <- arbitrary
    len <- arbitrary
    num <- arbitrary
    specials <- arbitrary

    return $
      WordCliOptions
        CliOptions
          { cliCapitals = caps,
            cliDigits = digits,
            cliLength = fromPositive len,
            cliMaxLength = Nothing,
            cliNumber = fromPositive num,
            cliPassphrase = False,
            cliSpecials = specials
          }
    where
      fromPositive = fmap getPositive

-- | CliOptions for generating only passphrases
newtype PhraseCliOptions = PhraseCliOptions {getPhraseOpts :: CliOptions}
  deriving (Eq)

instance Show PhraseCliOptions where
  show = show . getPhraseOpts

instance Arbitrary PhraseCliOptions where
  arbitrary = do
    caps <- arbitrary
    digits <- arbitrary
    minLen <- arbitrary
    maxLen <- arbitrary
    num <- arbitrary
    specials <- arbitrary

    -- CLI gets very inconsistent when len >= 80
    -- TODO: Fix ^^^
    let minLen' = fromLT40 minLen
        maxLen' = (+) <$> minLen' <*> fromLT40 maxLen

    return $
      PhraseCliOptions
        CliOptions
          { cliCapitals = caps,
            cliDigits = digits,
            cliLength = minLen',
            cliMaxLength = maxLen',
            -- CLI gets very inconsistent when number >= 20
            -- TODO: Fix ^^^
            cliNumber = fromLT20 num,
            cliPassphrase = True,
            cliSpecials = specials
          }

newtype LessThan20 n = LT20 {getLT20 :: n} deriving (Eq, Show)
newtype LessThan40 n = LT40 {getLT40 :: n} deriving (Eq, Show)
newtype LessThan79 n = LT79 {getLT79 :: n} deriving (Eq, Show)

instance (Arbitrary n, Num n, Random n) => Arbitrary (LessThan20 n) where
  arbitrary = LT20 <$> choose (1, 20)

instance (Arbitrary n, Num n, Random n) => Arbitrary (LessThan40 n) where
  arbitrary = LT40 <$> choose (1, 40)

instance (Arbitrary n, Num n, Random n) => Arbitrary (LessThan79 n) where
  arbitrary = LT79 <$> choose (1, 79)

-- | Convert CliOptions into a list of cli arguments
getOptions :: CliOptions -> [String]
getOptions opts =
  caps ++ digits ++ specials ++ num ++ phrase ++ len ++ maxLen
  where
    caps = ["-c" | cliCapitals opts]
    digits = ["-d" | cliDigits opts]
    len = maybe [] (singleton . show) (cliLength opts)
    maxLen = maybe [] (singleton . show) (cliMaxLength opts)
    maxLen' = cliLength opts >> cliMaxLength opts
    num = maybe [] (("-n" :) . singleton . show) (cliNumber opts)
    phrase = ["-p" | cliPassphrase opts]
    specials = ["-s" | cliSpecials opts]

-- | Run elocrypt with the given options
run :: CliOptions -> IO (Handle, Handle, Handle, ProcessHandle)
run opts = do
  res@(_, _, _, p) <- Proctest.run "elocrypt" (getOptions opts)
  sleep'
  _ <- assertExitedSuccess (seconds 2) p
  return res

-- Utilities
fromLT20 :: Functor f => f (LessThan20 a) -> f a
fromLT40 :: Functor f => f (LessThan40 a) -> f a
fromLT79 :: Functor f => f (LessThan79 a) -> f a
fromPositive :: Functor f => f (Positive a) -> f a
fromLT20 = fmap getLT20
fromLT40 = fmap getLT40
fromLT79 = fmap getLT79
fromPositive = fmap getPositive

readHandle :: Handle -> IO String
readHandle = (<$>) asUtf8Str . waitOutput (seconds 5) 5000

assertExitedSuccess :: Timeout -> ProcessHandle -> IO Bool
assertExitedSuccess t = fmap (== ExitSuccess) . assertExitedTimeout t

assertExitedFailure :: Timeout -> ProcessHandle -> IO Bool
assertExitedFailure t = fmap not . assertExitedSuccess t

sleep' :: IO ()
sleep' = sleep (seconds 0.0001)
