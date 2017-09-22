-- |A re-export of QuickCheck along with some useful instances
module Test.Elocrypt.QuickCheck (
  module Test.QuickCheck,
  CliOptions(..),
  PhraseCliOptions(..),
  WordCliOptions(..),
  getOptions
  ) where

import Data.List
import Data.Maybe
import Test.QuickCheck

data CliOptions = CliOptions {
  cliCapitals :: Bool,
  cliLength   :: Maybe Int,
  cliNumber   :: Maybe Int
} deriving Eq

instance Show CliOptions where
  show = unwords . getOptions

newtype WordCliOptions 
  = WordCliOptions { getWordOpts :: CliOptions }
  deriving Eq

instance Show WordCliOptions where
  show = show . getWordOpts

newtype PhraseCliOptions 
  = PhraseCliOptions { getPhraseOpts :: CliOptions }
  deriving Eq

instance Show PhraseCliOptions where
  show = show . getPhraseOpts

instance Arbitrary WordCliOptions where
  arbitrary = do
    caps <- arbitrary
    len  <- arbitrary
    num  <- arbitrary

    return $ WordCliOptions CliOptions{ 
      cliCapitals = caps,
      cliLength   = fromPositive len,
      cliNumber   = fromPositive num
    }
  
    where fromPositive = fmap getPositive
    
getOptions :: CliOptions -> [String]
getOptions CliOptions{cliCapitals=caps, cliLength=len, cliNumber=num}
  = caps' ++ num' ++ len'
  where caps' = ["-c" | caps]
        len'  = maybe [] (singleton . show) len
        num'  = maybe [] (("-n":) . singleton . show) num


-- Utilities
singleton :: a -> [a]
singleton a = [a]

