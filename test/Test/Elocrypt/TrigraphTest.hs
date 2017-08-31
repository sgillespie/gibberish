{-# LANGUAGE TemplateHaskell #-}
module Test.Elocrypt.TrigraphTest where

import Control.Monad
import Data.Char
import Data.Maybe
import Test.QuickCheck
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

import Test.Elocrypt.Instances
import Data.Elocrypt.Trigraph

tests = $(testGroupGenerator)

-- |Finding a frequency with 2 lowercase letters returns a trigraph
prop_findFrequencyWith2Letters :: LowerAlphaChar -> LowerAlphaChar -> Bool
prop_findFrequencyWith2Letters (LowerAlpha c1) (LowerAlpha c2)
  = isJust (findFrequency [c1, c2])

-- |Finding a frequency with more or less than 2 characters results in nothing
prop_findFrequencyWithNot2Letters :: String -> Property
prop_findFrequencyWithNot2Letters str
  = not (length str == 2 && all isLower str) ==> isNothing (findFrequency str)

-- |Finding a weight with 2 letters returns a weighted list
prop_findWeightsWith2Letters :: AlphaChar -> AlphaChar -> Bool
prop_findWeightsWith2Letters (Alpha c1) (Alpha c2)
  = isJust (findWeights [c1, c2])

-- |Finding a weight with more or less than 2 characters results in nothing
prop_findWeightsWithNot2Letters :: String -> Property
prop_findWeightsWithNot2Letters str
  = not (length str == 2 && all isLetter str) ==> isNothing (findWeights str)

-- |Finding a weight never results in zero weights
prop_findWeightsDefaultsFreqs :: AlphaChar -> AlphaChar -> Bool
prop_findWeightsDefaultsFreqs (Alpha c1) (Alpha c2)
  = sum' weights > 0
  where weights = fromJust . findWeights $ [c1, c2]
        sum' = foldr ((+) . snd) 0
