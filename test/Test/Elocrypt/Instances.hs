module Test.Elocrypt.Instances where

import Control.Monad
import System.Random
import Test.QuickCheck

instance Arbitrary StdGen where
  arbitrary = mkStdGen `liftM` arbitrary

newtype AlphaChar = Alpha Char
                  deriving (Eq, Ord, Show)

instance Arbitrary AlphaChar where
  arbitrary = Alpha `liftM` elements ['a'..'z']

newtype GreaterThan2 a = GT2 { greaterThan2 :: a }
                       deriving (Eq, Ord, Show)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (GreaterThan2 a) where
  arbitrary = GT2 `fmap` (arbitrary `suchThat` (>2))
