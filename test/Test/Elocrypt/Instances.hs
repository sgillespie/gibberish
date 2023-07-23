module Test.Elocrypt.Instances where

import Control.Monad
import System.Random
import Text.Printf

import Test.QuickCheck

-- | Representation of possible CLI arguments for a command line
--  app.
newtype CliArgs = CliArgs {getArgs :: [String]}
  deriving (Eq)

instance Show CliArgs where
  show = unwords . getArgs

instance Arbitrary CliArgs where
  arbitrary = do
    len <- arbitrary `suchThat` (> 0) `suchThat` (<= 40) :: Gen Int
    num <- arbitrary `suchThat` (> 2) `suchThat` (<= 20) :: Gen Int
    args <-
      sublistOf
        [ "-n %d" `printf` num,
          "-c",
          show len
        ]
    return (CliArgs args)

-- | Representation of an elocrypt execution with passphrases turned
--  on.
newtype PhraseCliArgs = PhraseCliArgs {getPhraseArgs :: [String]}
  deriving (Eq)

instance Show PhraseCliArgs where
  show = unwords . getPhraseArgs

instance Arbitrary PhraseCliArgs where
  arbitrary = do
    num <- arbitrary `suchThat` (> 2) `suchThat` (<= 20) :: Gen Int
    minLen <- arbitrary `suchThat` (> 0) `suchThat` (<= 40) :: Gen Int
    maxLen <- arbitrary `suchThat` (> 0) `suchThat` (<= 40) :: Gen Int

    -- Need Either [], [num], or [num, minLength, maxLength]
    args <-
      sublistOf
        [ "-n %d" `printf` num,
          show minLen,
          show (maxLen + minLen)
        ]

    return $ PhraseCliArgs ("-p" : args)

instance Arbitrary StdGen where
  arbitrary = mkStdGen `liftM` arbitrary

-- | All letters, upper and lower case.
newtype AlphaChar = Alpha Char
  deriving (Eq, Ord, Show)

instance Arbitrary AlphaChar where
  arbitrary = Alpha <$> elements (['a' .. 'z'] ++ ['A' .. 'Z'])

-- | All lowercase letters
newtype LowerAlphaChar = LowerAlpha Char
  deriving (Eq, Ord, Show)

instance Arbitrary LowerAlphaChar where
  arbitrary = LowerAlpha <$> elements ['a' .. 'z']

-- | DEPRECATED: Positive integers.
newtype GreaterThan0 a = GT0 {getGT0 :: a}
  deriving (Eq, Ord, Show)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (GreaterThan0 a) where
  arbitrary = GT0 `fmap` (arbitrary `suchThat` (> 0))

-- | DEPRECATED: Integers greater than 2
newtype GreaterThan2 a = GT2 {getGT2 :: a}
  deriving (Eq, Ord, Show)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (GreaterThan2 a) where
  arbitrary = GT2 <$> (arbitrary `suchThat` (> 2))

-- | DEPRECATED: Integers greater than 79
newtype GreaterThan79 a = GT79 {getGT79 :: a}
  deriving (Eq, Ord, Show)

instance (Integral a, Random a) => Arbitrary (GreaterThan79 a) where
  arbitrary = GT79 <$> choose (80, 500)
