{-# LANGUAGE TemplateHaskell #-}
module Elocrypt.TrigraphTest where

import Control.Monad
import Test.QuickCheck
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

import Elocrypt.Test.Instances
import Elocrypt.Trigraph

tests = $(testGroupGenerator)

prop_findFrequencyShouldSucceedWith2Chars :: AlphaChar -> AlphaChar -> Bool
prop_findFrequencyShouldSucceedWith2Chars (Alpha c1) (Alpha c2)
  = fromJust (findFrequency [c1, c2])
  where fromJust (Just _) = True
        fromJust Nothing  = False

prop_findFrequencyShouldFailWithNot2Chars :: String -> Property
prop_findFrequencyShouldFailWithNot2Chars str
  = length str /= 2 ==> findFrequency str == Nothing
