-- |A re-export of QuickCheck along with some useful instances
module Test.Elocrypt.QuickCheck (
  module Test.QuickCheck,
  CliOptions(..),
  LessThan20(..),
  LessThan40(..),
  LessThan79(..),
  PhraseCliOptions(..),
  WordCliOptions(..),
  getOptions,
  readHandle,
  run
  ) where

import Data.Bool
import Data.List
import Data.Maybe
import System.Random
import Test.QuickCheck
import Test.Proctest hiding (run)
import Test.Proctest.Assertions
import qualified Test.Proctest as Proctest

-- |A representation of elocrypt's command line
-- options
data CliOptions = CliOptions {
  cliCapitals   :: Bool,
  cliLength     :: Maybe Int,
  cliMaxLength  :: Maybe Int,
  cliNumber     :: Maybe Int,
  cliPassphrase :: Bool
} deriving Eq

instance Show CliOptions where
  show = unwords . getOptions

-- |CliOptions for generating only passwords
newtype WordCliOptions 
  = WordCliOptions { getWordOpts :: CliOptions }
  deriving Eq

instance Show WordCliOptions where
  show = show . getWordOpts

instance Arbitrary WordCliOptions where
  arbitrary = do
    caps <- arbitrary
    len  <- arbitrary
    num  <- arbitrary

    return $ WordCliOptions CliOptions{ 
      cliCapitals   = caps,
      cliLength     = fromPositive len,
      cliMaxLength  = Nothing,
      cliNumber     = fromPositive num,
      cliPassphrase = False
    }
  
    where fromPositive = fmap getPositive

-- |CliOptions for generating only passphrases
newtype PhraseCliOptions 
  = PhraseCliOptions { getPhraseOpts :: CliOptions }
  deriving Eq

instance Show PhraseCliOptions where
  show = show . getPhraseOpts

instance Arbitrary PhraseCliOptions where
  arbitrary = do
    caps   <- arbitrary
    minLen <- arbitrary
    maxLen <- arbitrary
    num    <- arbitrary

    -- CLI gets very inconsistent when len >= 80
    -- TODO: Fix ^^^
    let minLen' = fromLT40 minLen
        maxLen' = (+) <$> minLen' <*> fromLT40 maxLen

    return $ PhraseCliOptions CliOptions{ 
      cliCapitals   = caps,
      cliLength     = minLen',
      cliMaxLength  = maxLen',  
      -- CLI gets very inconsistent when number >= 20
      -- TODO: Fix ^^^
      cliNumber     = fromLT20 num,
      cliPassphrase = True
    }

newtype LessThan20 n = LT20 { getLT20 :: n } deriving (Eq, Show)
newtype LessThan40 n = LT40 { getLT40 :: n } deriving (Eq, Show)
newtype LessThan79 n = LT79 { getLT79 :: n } deriving (Eq, Show)

instance (Arbitrary n, Num n, Random n) => Arbitrary (LessThan20 n) where
  arbitrary = LT20 <$> choose (1, 20)

instance (Arbitrary n, Num n, Random n) => Arbitrary (LessThan40 n) where
  arbitrary = LT40 <$> choose (1, 40)

instance (Arbitrary n, Num n, Random n) => Arbitrary (LessThan79 n) where
  arbitrary = LT79 <$> choose (1, 79)

-- |Convert CliOptions into a list of cli arguments
getOptions :: CliOptions -> [String]
getOptions opts
  = caps ++ num ++ phrase ++ len ++ maxLen
  where caps   = ["-c" | cliCapitals opts]
        len    = maybe [] (singleton . show) (cliLength opts)
        maxLen = maybe [] (singleton . show) (cliMaxLength opts)
        maxLen' = cliLength opts >> cliMaxLength opts
        num    = maybe [] (("-n":) . singleton . show) (cliNumber opts)
        phrase = ["-p" | cliPassphrase opts]

-- |Run elocrypt with the given options
run :: CliOptions -> IO (Handle, Handle, Handle, ProcessHandle)
run opts = do
  res@(_, _, _, p) <- Proctest.run "elocrypt" (getOptions opts)
  sleep'
  _ <- assertExitedSuccess (seconds 2) p
  return res

-- Utilities
singleton :: a -> [a]
singleton a = [a]

fromLT20 :: Functor f => f (LessThan20 a) -> f a
fromLT40 :: Functor f => f (LessThan40 a) -> f a
fromLT79 :: Functor f => f (LessThan79 a) -> f a
fromPositive :: Functor f => f (Positive a) -> f a

fromLT20 = fmap getLT20
fromLT40 = fmap getLT40
fromLT79 = fmap getLT79
fromPositive = fmap getPositive

readHandle :: Handle -> IO String
readHandle = (<$>) asUtf8Str . waitOutput (seconds 2) 5000

assertExitedSuccess :: Timeout -> ProcessHandle -> IO Bool
assertExitedSuccess t = fmap (== ExitSuccess) . assertExitedTimeout t

assertExitedFailure :: Timeout -> ProcessHandle -> IO Bool
assertExitedFailure t = fmap not . assertExitedSuccess t

sleep' :: IO ()
sleep' = sleep (seconds 0.0001)
