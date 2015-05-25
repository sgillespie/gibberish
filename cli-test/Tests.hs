{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Functor
import Data.List
import Data.Maybe
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
  = isJust (getPosParam args) ==>
    ioProperty $ do
      (in', out', err', p) <- run' "dist/build/elocrypt/elocrypt" args
      response <- readHandle out'

      let len    = read . fromJust . getPosParam $ args
          words' = words response

      return (all ((==) len . length) words')

prop_shouldPrintNumberPasswords :: CliArgs -> Property
prop_shouldPrintNumberPasswords (CliArgs args)
  = isJust (getArg "-n" args) ==>
    ioProperty $ do
      (in', out', err', p) <- run' "dist/build/elocrypt/elocrypt" args
      response <- readHandle out'

      let number = read . fromJust . getArg "-n" $ args
          words' = words response
          
      return (number == length words')

prop_shouldPrintMultPasswordsPerLine :: CliArgs -> Property
prop_shouldPrintMultPasswordsPerLine (CliArgs args)
  = (read . fromMaybe "8" . getPosParam $ args) <= 38 ==>
    ioProperty $ do
      (in', out', err', p) <- run' "dist/build/elocrypt/elocrypt" args
      response <- readHandle out'
      return . all (>1) . tail . reverse . map length . map words . lines $ response

prop_shouldPrintDefaultMultPasswordsPerLine :: CliArgs -> Property
prop_shouldPrintDefaultMultPasswordsPerLine (CliArgs args)
  = isNothing (getArg "-n" args) ==>
    (read . fromMaybe "8" . getPosParam $ args) <= 38 ==>
    ioProperty $ do
      (in', out', err', p) <- run' "dist/build/elocrypt/elocrypt" args
      response <- readHandle out'
      return . all (>1) . tail . reverse . map length . map words . lines $ response

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

getArg :: String -> [String] -> Maybe String
getArg prefix args = (tail . dropWhile (not . elem')) `liftM` arg
  where arg = find (isPrefixOf prefix) args
        elem' = (flip elem) [' ', '=']

getPosParam :: [String] -> Maybe String
getPosParam = find $ (/= '-') . head
