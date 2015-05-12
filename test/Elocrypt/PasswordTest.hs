{-# LANGUAGE TemplateHaskell #-}
module Elocrypt.PasswordTest where

import Control.Monad
import Control.Monad.Random hiding (next)
import Data.Maybe
import System.Random hiding (next)
import Test.QuickCheck hiding (generate)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

import Elocrypt.Password
import Elocrypt.Trigraph
import Elocrypt.TrigraphTest -- TODO: Extract useful instances into common module

newtype GenPassword = GenPassword (String, StdGen)

instance Show GenPassword where
  show (GenPassword s) = fst s

instance Eq GenPassword where
  (GenPassword p1) == (GenPassword p2) = fst p1 == fst p2

instance Arbitrary StdGen where
  arbitrary = mkStdGen `liftM` arbitrary

instance Arbitrary GenPassword where
  arbitrary = do
    gen <- arbitrary
    len <- suchThat arbitrary (>=6)
    return . GenPassword . generate gen $ len

tests = $(testGroupGenerator)

prop_generateShouldBeUnique :: GenPassword -> Bool
prop_generateShouldBeUnique (GenPassword (p, g))
  = p /= (fst . generate g . length $ p)

prop_generateShouldBeLong :: GenPassword -> Bool
prop_generateShouldBeLong (GenPassword p) = length (fst p) >= 6

prop_generateShouldBeSameLength :: GenPassword -> Bool
prop_generateShouldBeSameLength (GenPassword (pass, gen))
  = len == (length . fst . generate gen $ len)
  where len = length pass

prop_generateShouldConsistOfAlphabet :: GenPassword -> Bool
prop_generateShouldConsistOfAlphabet (GenPassword (p, _))
  = all ((flip elem) alphabet) p

prop_first2ShouldHaveLength2 :: StdGen -> Bool
prop_first2ShouldHaveLength2 g = length (evalRand first2 g) == 2

prop_nextShouldSkip0Weights :: TrigraphChar -> TrigraphChar -> StdGen -> Property
prop_nextShouldSkip0Weights (T c1) (T c2) gen = isCandidate next' [c1, c2]
  where next' = evalRand (next . reverse $ [c1, c2]) gen

prop_lastNShouldSkip0Weights :: TrigraphChar -> TrigraphChar -> Int -> StdGen -> Property
prop_lastNShouldSkip0Weights (T c1) (T c2) len gen
  = len > 0 ==> lastNShouldSkip0Weights' lastN'
  where f2 = [c1, c2]
        lastN' = evalRand (lastN (reverse [c1, c2]) len) gen

lastNShouldSkip0Weights' :: String -> Property
lastNShouldSkip0Weights' (p:ps:pss:psss) = isCandidate p [pss, ps] .&&.
                                           lastNShouldSkip0Weights' (ps:pss:psss)
lastNShouldSkip0Weights' _ = property True

-- Utility functions
isCandidate :: Char -> String -> Property
isCandidate c (s:ss:sss) = hasCandidates f2 ==>
                           isJust $ elem c `liftM` findNextCandidates f2
  where f2 = [s, ss]

isCandidate _ _ = undefined -- This really shouldn't ever happen

hasCandidates :: String -> Bool
hasCandidates s = (not . null) `liftM` findNextCandidates s == Just True

findNextCandidates :: String -> Maybe [Char]
findNextCandidates (c1:c2:_) = map fst `liftM` frequency
  where f2 = [c1, c2]
        frequency = (filter ((/=0) . snd) . zip ['a'..'z']) `liftM` findFrequency f2

findNextCandidates _ = Nothing  -- This really shouldn't ever happen
