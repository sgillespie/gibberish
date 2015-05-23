{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Functor
import Data.List
import Data.Maybe (fromJust)
import Control.Monad

import Test.Proctest
import Test.Proctest.Assertions
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.Tasty hiding (Timeout)
import Test.Tasty.QuickCheck (QuickCheckTests(..), testProperty)
import Test.Tasty.TH

import Test.Elocrypt.Instances

main :: IO ()
main = defaultMain (options tests)

options :: TestTree -> TestTree
options = localOption (QuickCheckTests 10)

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
    len  <- arbitrary :: Gen (GreaterThan2 Int)
    num  <- arbitrary :: Gen (GreaterThan2 Int)
    args <- listOf (elements ["-n " ++ show (getGT2 num), "--number=" ++ show (getGT2 num),
                              show (getGT2 len)])
    return (CliArgs args)
              
prop_helpShouldPrintUsage :: CliArg -> Property
prop_helpShouldPrintUsage (CliArg arg)
  = arg == "-h" || arg == "--help" ==>
    ioProperty $ do
      (in', out', err', p) <- run "dist/build/elocrypt/elocrypt" [arg]
      sleep'
      assertExitedTimeout (seconds 2) p
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

prop_shouldPrintPasswordsWithLength :: CliArgs -> Property
prop_shouldPrintPasswordsWithLength (CliArgs args)
  = any (all ((flip elem) ['0'..'9'])) args  ==>
    ioProperty $ do
      (in', out', err', p) <- run "dist/build/elocrypt/elocrypt" args
      sleep'
      response <- asUtf8Str <$> waitOutput (seconds 2) 5000 out'
      assertExitedSuccess (seconds 2) p

      let len    = fromJust $ find (all ((flip elem) ['0'..'9'])) args
          words' = words response

      return (all (\s -> length s == read len) words')

prop_shouldPrintNumberPasswords :: CliArgs -> Property
prop_shouldPrintNumberPasswords (CliArgs args)
  = any (\s -> "-n" `isPrefixOf` s || "--number=" `isPrefixOf` s) args  ==>
    ioProperty $ do
      (in', out', err', p) <- run "dist/build/elocrypt/elocrypt" args
      sleep'
      response <- asUtf8Str <$> waitOutput (seconds 2) 5000 out'
      assertExitedSuccess (seconds 2) p

      let option = fromJust $ find (\s -> "-n" `isPrefixOf` s || "--number=" `isPrefixOf` s) args
          number = tail $ dropWhile (not . ((flip elem) [' ', '='])) option
          words' = words response

      return (read number == length words')

-- Utility functions
assertExitedSuccess :: Timeout -> ProcessHandle -> IO Bool
assertExitedSuccess t = liftM (== ExitSuccess) . assertExitedTimeout t

assertExitedFailure :: Timeout -> ProcessHandle -> IO Bool
assertExitedFailure t = liftM not . assertExitedSuccess t

sleep' :: IO ()
sleep' = sleep (seconds 0.0001)
