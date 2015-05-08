{-# LANGUAGE TemplateHaskell #-}
module Elocrypt.PasswordTest where

import Control.Monad
import System.Random
import Test.QuickCheck (Arbitrary, arbitrary)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

import Elocrypt.Password

newtype GenPassword = GenPassword (String, StdGen)

instance Show GenPassword where
  show (GenPassword s) = fst s

instance Eq GenPassword where
  (GenPassword p1) == (GenPassword p2) = fst p1 == fst p2

instance Arbitrary GenPassword where
  arbitrary = (GenPassword . generate . mkStdGen) `liftM` arbitrary

tests = $(testGroupGenerator)

prop_generateShouldBeUnique :: GenPassword -> Bool
prop_generateShouldBeUnique (GenPassword (p, g))
  = p /= fst (generate g)

prop_generateShouldBeLong :: GenPassword -> Bool
prop_generateShouldBeLong (GenPassword p) = length (fst p) >= 6
