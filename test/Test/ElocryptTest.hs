{-# LANGUAGE TemplateHaskell #-}
module Test.ElocryptTest where

import Control.Monad
import Control.Monad.Random hiding (next)
import Data.Maybe
import System.Random hiding (next)
import Test.QuickCheck
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

import Data.Elocrypt
import Data.Elocrypt.Trigraph
import Test.Elocrypt.Instances

tests = $(testGroupGenerator)

prop_genPasswordShouldBeUnique :: GreaterThan2 Int -> StdGen -> Bool
prop_genPasswordShouldBeUnique (GT2 len) gen
  = p /= fst (genPassword len gen')
  where (p, gen') = genPassword len gen

prop_genPasswordsShouldBeUnique :: GreaterThan2 Int -> StdGen -> Bool
prop_genPasswordsShouldBeUnique (GT2 len) gen
  = p /= ps
  where (p:ps:_) = fst (genPasswords len gen)

prop_newPasswordShouldBeLength :: GreaterThan2 Int -> StdGen -> Bool
prop_newPasswordShouldBeLength (GT2 len) gen = length (newPassword len gen) == len

prop_newPasswordShouldConsistOfAlphabet :: GreaterThan2 Int -> StdGen -> Bool
prop_newPasswordShouldConsistOfAlphabet (GT2 len) gen
  = all ((flip elem) alphabet) (newPassword len gen)

prop_newPasswordsShouldBeUnique :: GreaterThan2 Int -> StdGen -> Bool
prop_newPasswordsShouldBeUnique (GT2 len) gen
  = p /= ps
  where (p:ps:_) = newPasswords len gen

prop_first2ShouldHaveLength2 :: StdGen -> Bool
prop_first2ShouldHaveLength2 g = length (evalRand first2 g) == 2

prop_nextShouldSkip0Weights :: AlphaChar -> AlphaChar -> StdGen -> Property
prop_nextShouldSkip0Weights (Alpha c1) (Alpha c2) gen = isCandidate next' [c1, c2]
  where next' = evalRand (next . reverse $ [c1, c2]) gen

prop_lastNShouldSkip0Weights :: AlphaChar -> AlphaChar -> Int -> StdGen -> Property
prop_lastNShouldSkip0Weights (Alpha c1) (Alpha c2) len gen
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
