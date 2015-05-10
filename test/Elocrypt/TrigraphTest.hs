{-# LANGUAGE TemplateHaskell #-}
module Elocrypt.TrigraphTest where

import Control.Monad
import Test.QuickCheck
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

import Elocrypt.Trigraph

tests = $(testGroupGenerator)

newtype TrigraphChar = T Char
                     deriving (Eq, Show)

instance Arbitrary TrigraphChar where
  arbitrary = T `liftM` elements ['a'..'z']

prop_findFrequencyShouldSucceedWith2Chars :: TrigraphChar -> TrigraphChar -> Bool
prop_findFrequencyShouldSucceedWith2Chars (T c1) (T c2)
  = fromJust (findFrequency [c1, c2])
  where fromJust (Just _) = True
        fromJust Nothing  = False

prop_findFrequencyShouldFailWithNot2Chars :: String -> Property
prop_findFrequencyShouldFailWithNot2Chars str
  = length str /= 2 ==> findFrequency str == Nothing
