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

instance Arbitrary StdGen where
  arbitrary = mkStdGen `liftM` arbitrary

tests = $(testGroupGenerator)

prop_genPasswordShouldBeUnique :: Int -> StdGen -> Property
prop_genPasswordShouldBeUnique len gen
  = len >= 3 ==> p /= fst (genPassword len gen')
  where (p, gen') = genPassword len gen

prop_newPasswordShouldBeLength :: Int -> StdGen -> Property
prop_newPasswordShouldBeLength len gen = len >= 3 ==> length (newPassword len gen) == len

prop_newPasswordShouldConsistOfAlphabet :: Int -> StdGen -> Property
prop_newPasswordShouldConsistOfAlphabet len gen
  = len >= 3 ==> all ((flip elem) alphabet) (newPassword len gen)

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
