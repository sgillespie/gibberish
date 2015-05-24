{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Functor
import Data.List
import Data.Maybe (fromJust, fromMaybe)
import Control.Monad
import Text.Printf

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

newtype CliArgs = CliArgs  { getArgs :: [String] }
               deriving Eq

instance Show CliArgs where
  show = concat . intersperse " " . getArgs

instance Arbitrary CliArgs where
  arbitrary = do
    len  <- arbitrary `suchThat` (>2) `suchThat` (<=40) :: Gen Int
    num  <- arbitrary `suchThat` (>2) `suchThat` (<=20) :: Gen Int
    args <- sublistOf ["-n %d" `printf` num,
                       show len]
    return (CliArgs args)
              
prop_shouldPrintPasswordsWithLength :: CliArgs -> Property
prop_shouldPrintPasswordsWithLength (CliArgs args)
  = any (all ((flip elem) ['0'..'9'])) args  ==>
    ioProperty $ do
      (in', out', err', p) <- run' "dist/build/elocrypt/elocrypt" args
      response <- readHandle out'

      let len    = fromJust $ find (all ((flip elem) ['0'..'9'])) args
          words' = words response

      return (all (\s -> length s == read len) words')

prop_shouldPrintNumberPasswords :: CliArgs -> Property
prop_shouldPrintNumberPasswords (CliArgs args)
  = any (isPrefixOf "-n") args  ==>
    ioProperty $ do
      (in', out', err', p) <- run' "dist/build/elocrypt/elocrypt" args
      response <- readHandle out'

      let option = fromJust $ find (isPrefixOf "-n") args
          number = tail $ dropWhile (not . ((flip elem) [' ', '='])) option
          words' = words response

      return (read number == length words')

prop_shouldPrintMultPasswordsPerLine :: CliArgs -> Property
prop_shouldPrintMultPasswordsPerLine (CliArgs args)
  = let len = fromMaybe "8" (find (all ((flip elem) ['0'..'9'])) args)
    in read len <= 38 ==> (ioProperty $ do
      (in', out', err', p) <- run' "dist/build/elocrypt/elocrypt" args
      response <- readHandle out'
      return . all (>1) . tail . reverse . map length . map words . lines $ response)

-- Utility functions
run' :: FilePath -> [String] -> IO (Handle, Handle, Handle, ProcessHandle)
run' exe args = do
  res@(in', out', err', p) <- run exe args
  sleep'
  assertExitedSuccess (seconds 2) p
  return res

readHandle :: Handle -> IO String
readHandle = (<$>) asUtf8Str . waitOutput (seconds 2) 5000

assertExitedSuccess :: Timeout -> ProcessHandle -> IO Bool
assertExitedSuccess t = liftM (== ExitSuccess) . assertExitedTimeout t

assertExitedFailure :: Timeout -> ProcessHandle -> IO Bool
assertExitedFailure t = liftM not . assertExitedSuccess t

sleep' :: IO ()
sleep' = sleep (seconds 0.0001)
