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
prop_printsPasswordsWithLength :: WordCliOptions -> Property
prop_printsPasswordsWithLength (WordCliOptions opts)
  = isJust len ==>
    ioProperty $ do
      (in', out', err', p) <- run' elocrypt (getOptions opts)
      response <- readHandle out'

      let len'   = fromJust len
          words' = words response

      return (all ((==) len' . length) words')

  where CliOptions{cliLength=len} = opts

-- |Prints nothing when length is 0
prop_printsNothingWhenLengthIsZero :: WordCliOptions -> Property
prop_printsNothingWhenLengthIsZero (WordCliOptions opts)
  = ioProperty $ do
      let opts' = opts { cliLength = Just 0 }

      (in', out', err', p) <- run' elocrypt (getOptions opts')
      response <- readHandle out'
      return (response == "")

-- |Always prints at least 1 password
prop_printsLongPasswords :: WordCliOptions -> Property
prop_printsLongPasswords (WordCliOptions opts)
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
prop_printsNumberPasswords :: Positive Int -> WordCliOptions -> Property
prop_printsNumberPasswords (Positive num) (WordCliOptions opts)
  = ioProperty $ do
      let opts' = opts { cliNumber = Just num }

      (in', out', err', p) <- run' elocrypt (getOptions opts')
      response <- readHandle out'

      let words' = words response

      return $ num == length words'

-- |Prints multiple passwords per line when length is sufficiently small
prop_printsMultiplePasswordsPerLine :: WordCliOptions -> Property
prop_printsMultiplePasswordsPerLine (WordCliOptions opts)
  = isNothing len || fromJust len <= 38 ==>
    ioProperty $ do
      (in', out', err', p) <- run' elocrypt (getOptions opts)
      response <- readHandle out'

      return $
        all (>1) . tail . reverse . map (length . words) . lines $ response

  where CliOptions{cliLength=len} = opts

-- |Prints capitals when specified
prop_printsCapitals :: WordCliOptions -> Property
prop_printsCapitals (WordCliOptions opts)
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
