-- |A re-export of QuickCheck along with some useful instances
module Test.Elocrypt.QuickCheck (
  module Test.QuickCheck,
  CliOptions(..),
  getOptions
  ) where

import Data.List
import Data.Maybe
import Test.QuickCheck

data CliOptions = CliOptions {
  cliCapitals :: Bool,
  cliLength   :: Maybe Int,
  cliNumber   :: Maybe Int
} deriving (Eq)

instance Arbitrary CliOptions where
  arbitrary = do
    caps <- arbitrary
    len  <- arbitrary
    num  <- arbitrary

    return CliOptions { 
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

instance Show CliOptions where
  show = unwords . getOptions

-- Utilities
singleton :: a -> [a]
singleton a = [a]

