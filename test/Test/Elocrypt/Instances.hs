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

newtype GreaterThan0 a = GT0 { getGT0 :: a }
  deriving (Eq, Ord, Show)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (GreaterThan0 a) where
  arbitrary = GT0 `fmap` (arbitrary `suchThat` (>0))

newtype GreaterThan2 a = GT2 { getGT2 :: a }
                       deriving (Eq, Ord, Show)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (GreaterThan2 a) where
  arbitrary = GT2 `fmap` (arbitrary `suchThat` (>2))

newtype GreaterThan79 a = GT79 { getGT79 :: a }
                        deriving (Eq, Ord, Show)

instance (Integral a, Random a) => Arbitrary (GreaterThan79 a) where
  arbitrary = GT79 `fmap` choose (80, 500)
