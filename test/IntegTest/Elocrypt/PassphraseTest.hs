{-# LANGUAGE TemplateHaskell #-}

module IntegTest.Elocrypt.PassphraseTest where

import Control.Monad
import Data.Bool
import Data.Char hiding (isSymbol)
import Data.List
import Data.Maybe

import qualified Test.Proctest as Proctest
import Test.QuickCheck
import Test.Tasty hiding (Timeout)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

import Data.Elocrypt.Utils (isSymbol)
import Test.Elocrypt.QuickCheck

tests :: TestTree
tests = $(testGroupGenerator)

elocrypt = "elocrypt"

-- | Passphrases consist of words in specified length range
prop_printsWordsWithLengthRange :: PhraseCliOptions -> Property
prop_printsWordsWithLengthRange (PhraseCliOptions opts) =
  isJust minLen
    && isJust maxLen
    && fromJust maxLen
      <= 79
        ==> ioProperty
    $ do
      (_, out, _, _) <- run opts
      response <- readHandle out

      let words' = words response
          minLen' = fromJust minLen
          maxLen' = fromJust maxLen

      return (all ((\n -> n >= minLen' && n <= maxLen') . length) words')
  where
    CliOptions {cliLength = minLen, cliMaxLength = maxLen} = opts

-- | Passphrases consist of words with specified minimum range
prop_printsWordsWithMinLength :: PhraseCliOptions -> Property
prop_printsWordsWithMinLength (PhraseCliOptions opts) =
  isJust minLen
    && fromJust minLen
      <= 79
        ==> ioProperty
    $ do
      (_, out, _, _) <- run opts
      response <- readHandle out

      return . all ((>= min (fromJust minLen) 10) . length) . words $ response
  where
    CliOptions {cliLength = minLen} = opts

-- | Prints the specificed number of phrases
prop_printsSpecifiedNumberOfPassphrases :: PhraseCliOptions -> Property
prop_printsSpecifiedNumberOfPassphrases (PhraseCliOptions opts) =
  isJust number
    && (isJust maxLen || isNothing len)
    && maybe True (< 79) maxLen -- maxLen > len
      ==> ioProperty -- minLen/maxLen < 79
    $ do
      (_, out, _, _) <- run opts
      response <- readHandle out

      let phrases = lines response

      return $
        counterexample
          (failMsg phrases)
          (fromJust number == length phrases)
  where
    CliOptions {cliNumber = number, cliLength = len, cliMaxLength = maxLen} = opts
    failMsg p =
      "length phrases ("
        ++ show (length p)
        ++ ") /= "
        ++ show (fromJust number)

-- | Prints multiple passwords per line
prop_printsMultipleWordsPerLine
  :: PhraseCliOptions
  -> LessThan20 Int
  -> LessThan20 Int
  -> Property
prop_printsMultipleWordsPerLine (PhraseCliOptions opts) (LT20 min) (LT20 max) =
  ioProperty $ do
    let opts' =
          opts -- Make sure we can fit 2+ words on a line
            { cliLength = Just min,
              cliMaxLength = Just max
            }

    (_, out, _, _) <- run opts'
    response <- readHandle out

    return $
      all (> 1) . tail . reverse . map (length . words) . lines $
        response

-- | Always prints a passphrase
prop_printsLongPassphrases
  :: PhraseCliOptions
  -> Positive Int
  -> Positive Int
  -> Property
prop_printsLongPassphrases (PhraseCliOptions opts) (Positive min) (Positive max) =
  forAll (scale (* 7) arbitrary) $ \min ->
    ioProperty $ do
      let minLen = getPositive min
          opts' =
            opts
              { cliLength = Just minLen,
                cliMaxLength = Just (minLen + max)
              }

      (_, out, _, _) <- run opts
      response <- readHandle out

      return $
        cover 20 (minLen > 80) "long" $
          (not . any . (null . words)) . lines $
            response

-- | Prints capitals when specified
prop_printsCapitals :: PhraseCliOptions -> Property
prop_printsCapitals (PhraseCliOptions opts) =
  ioProperty $ do
    let opts' = opts {cliCapitals = True}

    (_, out, _, _) <- run opts'
    response <- readHandle out

    let result = any (any isUpper) . lines $ response

    return $
      cover 80 result "has caps" True

-- | Prints numbers when specified
prop_printsNumbers :: PhraseCliOptions -> Property
prop_printsNumbers (PhraseCliOptions opts) =
  ioProperty $ do
    let opts' = opts {cliDigits = True}

    (_, out, _, _) <- run opts'
    response <- readHandle out

    let result = any (any isDigit) . lines $ response

    return $
      cover 80 result "has numbers" True

-- | Prints special characters when specified
prop_printsSpecials :: PhraseCliOptions -> Property
prop_printsSpecials (PhraseCliOptions opts) = ioProperty $ do
  let opts' = opts {cliSpecials = True}

  (_, out, _, _) <- run opts'
  response <- readHandle out

  let result = any (any isSymbol) . lines $ response

  return $
    checkCoverage $
      cover 80 result "has special characters" True
