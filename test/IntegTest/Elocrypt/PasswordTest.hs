{-# LANGUAGE TemplateHaskell #-}
module IntegTest.Elocrypt.PasswordTest where

import Data.Char
import Data.List
import Data.Maybe
import Control.Monad

import Test.Elocrypt.QuickCheck
import Test.Proctest
import Test.Proctest.Assertions
import Test.Tasty hiding (Timeout)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

import Test.Elocrypt.Instances

tests :: TestTree
tests = $(testGroupGenerator)

elocrypt = "elocrypt"

-- |All passwords have specified length
prop_printsPasswordsWithLength :: CliOptions -> Property
prop_printsPasswordsWithLength opts@CliOptions {cliLength=len}
  = isJust len ==>
    ioProperty $ do
      (in', out', err', p) <- run' elocrypt (getOptions opts)
      response <- readHandle out'

      let len'   = fromJust len
          words' = words response

      return (all ((==) len' . length) words')

-- |Prints nothing when length is 0
prop_printsNothingWhenLengthIsZero :: CliOptions -> Property
prop_printsNothingWhenLengthIsZero opts
  = ioProperty $ do
      let opts' = opts { cliLength = Just 0 }

      (in', out', err', p) <- run' elocrypt (getOptions opts')
      response <- readHandle out'
      return (response == "")

-- |Always prints at least 1 password
prop_printsLongPasswords :: CliOptions -> Property
prop_printsLongPasswords opts
  = forAll (scale (*7) (arbitrary :: Gen (Positive Int))) $ \len ->
      ioProperty $ do
        let len'  = getPositive len
            opts' = opts { cliLength = Just len' }

        (in', out', err', p) <- run' elocrypt (getOptions opts')
        response <- readHandle out'

        return $
          cover (len' > 80) 30 "long" $
            all (>=1) . map (length . words) . lines $ response

-- |Prints the specified number of passwords
prop_printsNumberPasswords :: Positive Int -> CliOptions -> Property
prop_printsNumberPasswords (Positive num) opts
  = ioProperty $ do
      let opts' = opts { cliNumber = Just num }

      (in', out', err', p) <- run' elocrypt (getOptions opts')
      response <- readHandle out'

      let words' = words response

      return $ num == length words'

-- |Prints multiple passwords per line when length is sufficiently small
prop_printsMultiplePasswordsPerLine :: CliOptions -> Property
prop_printsMultiplePasswordsPerLine opts@CliOptions{cliLength=len}
  = isNothing len || fromJust len <= 38 ==>
    ioProperty $ do
      (in', out', err', p) <- run' elocrypt (getOptions opts)
      response <- readHandle out'

      return $
        all (>1) . tail . reverse . map (length . words) . lines $ response

-- |Prints capitals when specified
prop_printsCapitals :: CliOptions -> Property
prop_printsCapitals opts
  = ioProperty $ do
      let opts' = opts { cliCapitals = True }

      (in', out', err', p) <- run' elocrypt (getOptions opts')
      response <- readHandle out'

      let passes = words response

      return $
        cover (any (any isUpper) passes) 80 "has caps" True


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
assertExitedSuccess t = fmap (== ExitSuccess) . assertExitedTimeout t

assertExitedFailure :: Timeout -> ProcessHandle -> IO Bool
assertExitedFailure t = fmap not . assertExitedSuccess t

sleep' :: IO ()
sleep' = sleep (seconds 0.0001)

getArg :: String -> [String] -> Maybe String
getArg prefix args = (tail . dropWhile (not . elem')) `liftM` arg
  where arg = find (isPrefixOf prefix) args
        elem' = flip elem [' ', '=']

getPosParam :: [String] -> Maybe String
getPosParam = find $ (/= '-') . head
