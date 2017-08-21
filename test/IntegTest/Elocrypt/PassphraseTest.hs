{-# LANGUAGE TemplateHaskell #-}
module IntegTest.Elocrypt.PassphraseTest where

import Control.Monad
import Data.List
import Data.Maybe

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

prop_printsWordsWithSpecifiedLengthRange :: PhraseCliArgs -> Property
prop_printsWordsWithSpecifiedLengthRange (PhraseCliArgs args)
  = length (getPosParams args) == 2 ==>
    ioProperty $ do
      (_, out', _, _) <- run' elocrypt args
      response <- readHandle out'

      let (minLen:maxLen:_) = map read (getPosParams args)
          words' = words response

      return (all ((\n -> n >= minLen && n <= maxLen) . length) words')

prop_printsWordsWithSpecifiedMinLengthRange :: PhraseCliArgs -> Property
prop_printsWordsWithSpecifiedMinLengthRange (PhraseCliArgs args)
  = length (getPosParams args) == 1 ==>
    ioProperty $ do
      (_, out', _, _) <- run' elocrypt args
      response <- readHandle out'

      let (minLen:_) = map read (getPosParams args)

      return . all ((>= min minLen 10) . length) . words $ response

prop_printsWordsWithDefaultLengthRange :: PhraseCliArgs -> Property
prop_printsWordsWithDefaultLengthRange (PhraseCliArgs args)
  = length (getPosParams args) == 0 ==>
    ioProperty $ do
      (_, out', _, _) <- run' elocrypt args
      response <- readHandle out'

      let words' = words response

      return (all ((\n -> n >= 8 && n <= 10) . length) words')

prop_printsSpecifiedNumberOfPassphrases :: PhraseCliArgs -> Property
prop_printsSpecifiedNumberOfPassphrases (PhraseCliArgs args)
  = isJust (getArg "-n" args) &&
    length (getPosParams args) /= 1 ==>    -- Make sure maxLen > minLen
    ioProperty $ do
      (_, out', _, _) <- run' elocrypt args
      response        <- readHandle out'

      let number = read . fromJust . getArg "-n" $ args
          phrases = lines response

      return (number == length phrases)

prop_printsMultipleWordsPerLine :: PhraseCliArgs -> Property
prop_printsMultipleWordsPerLine (PhraseCliArgs args)
  = sum (map read (getPosParams args) :: [Integer]) <= 29 ==>
    ioProperty $ do
      (_, out', _, _) <- run' elocrypt args
      response <- readHandle out'

      return $
        all (>1) . tail . reverse . map (length . words) . lines $ response

prop_printsLongPassphrases :: GreaterThan79 Int -> GreaterThan0 Int -> Property
prop_printsLongPassphrases (GT79 minLen) (GT0 maxLen)
  = ioProperty $ do
    (_, out', _, _) <- run' elocrypt ["-p",
                                      show minLen,
                                      show (minLen + maxLen)]
    response        <- readHandle out'
    
    return . all (==1) . map (length . words) . lines $ response

getArg :: String -> [String] -> Maybe String
getArg prefix args = (tail . dropWhile (not . elem')) `liftM` arg
  where arg = find (isPrefixOf prefix) args
        elem' = flip elem [' ', '=']

getPosParams :: [String] -> [String]
getPosParams = filter ((/= '-') . head)

run' :: FilePath -> [String] -> IO (Handle, Handle, Handle, ProcessHandle)
run' exe args = do
  res@(_, _, _, p) <- run exe args
  sleep'
  _ <- assertExitedSuccess (seconds 2) p
  return res

readHandle :: Handle -> IO String
readHandle = (<$>) asUtf8Str . waitOutput (seconds 2) 5000

sleep' :: IO ()
sleep' = sleep (seconds 0.0001)

assertExitedSuccess :: Timeout -> ProcessHandle -> IO Bool
assertExitedSuccess t = fmap (== ExitSuccess) . assertExitedTimeout t

assertExitedFailure :: Timeout -> ProcessHandle -> IO Bool
assertExitedFailure t = fmap not . assertExitedSuccess t
