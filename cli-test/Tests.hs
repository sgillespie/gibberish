{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Functor
import Data.List
import Control.Monad

import Test.Proctest
import Test.Proctest.Assertions
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.Tasty hiding (Timeout)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "CLI Tests" [testGroup']

testGroup' = $(testGroupGenerator)

newtype CliArg = CliArg { getArg :: String }
               deriving Eq

instance Show CliArg where
  show = getArg

instance Arbitrary CliArg where
  arbitrary = do
    arg <- elements ["-h", "--help",
                     "-v", "--version"]
    return (CliArg arg)

newtype CliArgs = CliArgs  { getArgs :: [String] }
               deriving Eq

instance Show CliArgs where
  show = concat . intersperse " " . getArgs

instance Arbitrary CliArgs where
  arbitrary = do
    args <- listOf (elements ["-h", "--help",
                              "-v", "--version"])
    return (CliArgs args)
              
prop_helpShouldPrintUsage :: CliArg -> Property
prop_helpShouldPrintUsage (CliArg arg)
  = arg == "-h" || arg == "--help" ==>
    ioProperty $ do
      (in', out', err', p) <- run "dist/build/elocrypt/elocrypt" [arg]
      sleep'
      response <- asUtf8Str <$> waitOutput (seconds 2) 1000 err'
      return $ "Usage: " `isPrefixOf` response

prop_helpShouldExitSuccess :: CliArg -> Property
prop_helpShouldExitSuccess (CliArg arg)
  = arg == "-h" || arg == "--help" ==>
    ioProperty $ do
      (in', out', err', p) <- run "dist/build/elocrypt/elocrypt" [arg]
      sleep'
      assertExitedSuccess (seconds 2) p

prop_versionShouldExitSuccess :: CliArg -> Property
prop_versionShouldExitSuccess (CliArg arg)
  = arg == "-v" || arg == "--version" ==>
    ioProperty $ do
      (in', out', err', p) <- run "dist/build/elocrypt/elocrypt" [arg]
      sleep'
      assertExitedSuccess (seconds 2) p

-- Utility functions
assertExitedSuccess :: Timeout -> ProcessHandle -> IO Bool
assertExitedSuccess t = liftM (== ExitSuccess) . assertExitedTimeout t

assertExitedFailure :: Timeout -> ProcessHandle -> IO Bool
assertExitedFailure t = liftM not . assertExitedSuccess t

sleep' :: IO ()
sleep' = sleep (seconds 0.005)
