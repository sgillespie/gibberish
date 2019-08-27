{-# LANGUAGE TemplateHaskell #-}
module Test.Elocrypt.UtilsTest where

import Control.Monad.Random
import Data.Maybe (isNothing)
import Data.List (find, elem, sort)
import Data.Ratio ((%))
import qualified Data.Map as M

import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO)
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

import Data.Elocrypt.Utils
import Test.Elocrypt.QuickCheck
import Test.Elocrypt.Instances

tests :: TestTree
tests = $(testGroupGenerator)

-- |Maps letters to all digits 0-9
prop_numeralConversionsHasAllDigits :: Bool
prop_numeralConversionsHasAllDigits
  = sort digits == ['0'..'9']
  where digits = M.foldr (++) [] numeralConversions

-- |toDigit never returns a letter that looks like a number
prop_toDigitDoesntLookLikeNumber :: Char -> Bool
prop_toDigitDoesntLookLikeNumber c
  = isNothing $ find (`elem` candidates) c'
  where candidates = M.keys numeralConversions
        c' = toDigit c

-- |updateR always updates when prob = 1/0
prop_updateRAlwaysUpdates :: String -> StdGen -> Bool
prop_updateRAlwaysUpdates s = evalRand $ do
  let f _ = return '*'
      prob = 99999 % 1 -- Make prob absurdly high since we can't use 0 denoms
  s' <- updateR f prob s

  -- Make sure almost all letters are '*'
  return $ length (filter (=='*') s') >= length s - 1

-- |updateR never updates when prob = 0/1
prop_updateRNeverUpdates :: String -> StdGen -> Bool
prop_updateRNeverUpdates s = evalRand $ do
  let f _ = return '*'
      prob = 0 % 1
  s' <- updateR f prob s

  return $ s == s'

-- |update1 Always updates at least one character
prop_update1AlwaysUpdates1 :: String -> Property
prop_update1AlwaysUpdates1 s = not (null s) ==> do
  let f = (const . return) '*'
  
  pos <- choose (0, length s - 1)
  s'  <- update1 f s pos

  return $ elem '*' s'

-- |update1 allows empty string
prop_update1DoesntCrashOnEmpty :: Property
prop_update1DoesntCrashOnEmpty = monadicIO $ do
  let f _ = return '*'
  s <- update1 f "" 0

  return (s == "")
