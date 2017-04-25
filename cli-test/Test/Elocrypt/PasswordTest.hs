{-# LANGUAGE TemplateHaskell #-}
module Test.Elocrypt.PasswordTest where

import Data.List
import Data.Maybe
import Control.Monad

import Test.Proctest
import Test.Proctest.Assertions
import Test.QuickCheck
import Test.Tasty hiding (Timeout)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

import Test.Elocrypt.Instances

tests :: TestTree
tests = $(testGroupGenerator)

elocrypt = "elocrypt"

prop_printsPasswordsWithSpecifiedLength :: CliArgs -> Property
prop_printsPasswordsWithSpecifiedLength (CliArgs args)
  = isJust (getPosParam args) ==>
    ioProperty $ do
      (in', out', err', p) <- run' elocrypt args
      response <- readHandle out'

      let len    = read . fromJust . getPosParam $ args
          words' = words response

      return (all ((==) len . length) words')

prop_printsNothingWhenSpecifiedLengthIsZero :: CliArgs -> Property
prop_printsNothingWhenSpecifiedLengthIsZero (CliArgs args)
  = isNothing (getPosParam args) ==>
    ioProperty $ do
      let args' = args ++ ["0"]

      (in', out', err', p) <- run' elocrypt args'
      response <- readHandle out'
      return (response == "")

prop_printsLongPasswords :: GreaterThan79 Int -> Property
prop_printsLongPasswords (GT79 a)
  = ioProperty $ do
      (in', out', err', p) <- run' elocrypt [show a]
      response <- readHandle out'
      return . all (==1) . map (length . words) . lines $ response

prop_printsSpecifiedNumberOfPasswords :: CliArgs -> Property
prop_printsSpecifiedNumberOfPasswords (CliArgs args)
  = isJust (getArg "-n" args) ==>
    ioProperty $ do
      (in', out', err', p) <- run' elocrypt args
      response <- readHandle out'

      let number = read . fromJust . getArg "-n" $ args
          words' = words response

      return (number == length words')

prop_printsMultiplePasswordsPerLine :: CliArgs -> Property
prop_printsMultiplePasswordsPerLine (CliArgs args)
  = (read . fromMaybe "8" . getPosParam $ args) <= 38 ==>
    ioProperty $ do
      (in', out', err', p) <- run' elocrypt args
      response <- readHandle out'

      return $
        all (>1) . tail . reverse . map (length . words) . lines $ response

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
