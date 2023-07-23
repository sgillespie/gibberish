{-# LANGUAGE TemplateHaskell #-}

module IntegTest.Elocrypt.PasswordTest where

import Control.Monad
import Data.Char hiding (isSymbol)
import Data.List
import Data.Maybe

import Test.Tasty hiding (Timeout)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

import Data.Elocrypt.Utils
import Test.Elocrypt.QuickCheck

tests :: TestTree
tests = $(testGroupGenerator)

elocrypt = "elocrypt"

-- | All passwords have specified length
prop_printsPasswordsWithLength :: WordCliOptions -> Property
prop_printsPasswordsWithLength (WordCliOptions opts) =
  isJust len
    ==> ioProperty
    $ do
      (_, out, _, _) <- run opts
      response <- readHandle out

      let len' = fromJust len
          words' = words response

      return (all ((==) len' . length) words')
  where
    CliOptions {cliLength = len} = opts

-- | Prints nothing when length is 0
prop_printsNothingWhenLengthIsZero :: WordCliOptions -> Property
prop_printsNothingWhenLengthIsZero (WordCliOptions opts) =
  ioProperty $ do
    let opts' = opts {cliLength = Just 0}

    (_, out, _, _) <- run opts'
    response <- readHandle out
    return (response == "")

-- | Always prints at least 1 password
prop_printsLongPasswords :: WordCliOptions -> Property
prop_printsLongPasswords (WordCliOptions opts) =
  forAll (scale (* 7) arbitrary) $ \len ->
    ioProperty $ do
      let len' = getPositive len
          opts' = opts {cliLength = Just len'}

      (_, out, _, _) <- run opts'
      response <- readHandle out

      return $
        checkCoverage $
          cover 30 (len' > 80) "long" $
            all (>= 1) . map (length . words) . lines $
              response

-- | Prints the specified number of passwords
prop_printsNumberPasswords :: Positive Int -> WordCliOptions -> Property
prop_printsNumberPasswords (Positive num) (WordCliOptions opts) =
  ioProperty $ do
    let opts' = opts {cliNumber = Just num}

    (_, out, _, _) <- run opts'
    response <- readHandle out

    let words' = words response

    return $ num == length words'

-- | Prints multiple passwords per line when length is sufficiently small
prop_printsMultiplePasswordsPerLine :: WordCliOptions -> Property
prop_printsMultiplePasswordsPerLine (WordCliOptions opts) =
  isNothing len
    || fromJust len
      <= 38
        ==> ioProperty
    $ do
      (_, out, _, _) <- run opts
      response <- readHandle out

      return $
        all (> 1) . tail . reverse . map (length . words) . lines $
          response
  where
    CliOptions {cliLength = len} = opts

-- | Prints capitals when specified
prop_printsCapitals :: WordCliOptions -> Property
prop_printsCapitals (WordCliOptions opts) =
  ioProperty $ do
    let opts' = opts {cliCapitals = True}

    (_, out, _, _) <- run opts'
    response <- readHandle out

    let result = any (any isUpper) . words $ response

    return $
      cover 80 result "has caps" True

-- | Prints numbers when specified
prop_printsNumbers :: WordCliOptions -> Property
prop_printsNumbers (WordCliOptions opts) =
  ioProperty $ do
    let opts' = opts {cliDigits = True}

    (_, out, _, _) <- run opts'
    response <- readHandle out

    let result = any (any isDigit) . words $ response

    return $
      checkCoverage $
        cover 60 result "has numbers" True

-- | Prints special characters when specified
prop_printsSpecials :: WordCliOptions -> Property
prop_printsSpecials (WordCliOptions opts) = ioProperty $ do
  let opts' = opts {cliSpecials = True}

  (_, out, _, _) <- run opts'
  response <- readHandle out

  let result = any (any isSymbol) . words $ response

  return $
    checkCoverage $
      cover 60 result "has special characters" True
