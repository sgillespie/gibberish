{-# LANGUAGE TemplateHaskell #-}
module Test.ElocryptTest where

import Control.Monad
import Control.Monad.Random hiding (next)
import Data.Maybe
import Test.QuickCheck hiding (frequency)
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

import Data.Elocrypt
import Data.Elocrypt.Trigraph
import Test.Elocrypt.Instances

tests :: TestTree
tests = $(testGroupGenerator)

prop_genPasswordShouldBeUnique :: GreaterThan2 Int -> Bool -> StdGen -> Bool
prop_genPasswordShouldBeUnique (GT2 len) caps gen
  = p /= fst (genPassword len caps gen')
  where (p, gen') = genPassword len caps gen

prop_genPasswordsShouldBeUnique
  :: GreaterThan2 Int
  -> GreaterThan2 Int
  -> Bool
  -> StdGen
  -> Bool
prop_genPasswordsShouldBeUnique (GT2 len) (GT2 n) caps gen
  = p /= ps
  where (p:ps:_) = fst (genPasswords len n caps gen)

prop_newPasswordShouldBeLength :: Int -> Bool -> StdGen -> Property
prop_newPasswordShouldBeLength len caps gen = len > 0 ==>
                                              length (newPassword len caps gen) == len

prop_newPasswordShouldConsistOfAlphabet :: Int -> Bool -> StdGen -> Bool
prop_newPasswordShouldConsistOfAlphabet len caps gen
  = all (`elem` alphabet) (newPassword len caps gen)

prop_newPasswordsShouldBeUnique
  :: GreaterThan2 Int
  -> GreaterThan2 Int
  -> Bool
  -> StdGen
  -> Bool
prop_newPasswordsShouldBeUnique (GT2 len) (GT2 n) caps gen
  = p /= ps
  where (p:ps:_) = newPasswords len n caps gen

prop_newPasswordShouldHaveLen :: Int -> Bool -> StdGen -> Property
prop_newPasswordShouldHaveLen len caps gen
  = len >= 0 ==> length (newPassword len caps gen) == len

prop_first2ShouldHaveLength2 :: StdGen -> Bool
prop_first2ShouldHaveLength2 g = length (evalRand first2 g) == 2

prop_nextShouldSkip0Weights :: AlphaChar -> AlphaChar -> StdGen -> Property
prop_nextShouldSkip0Weights (Alpha c1) (Alpha c2) gen = isCandidate next' [c1, c2]
  where next' = evalRand (next . reverse $ [c1, c2]) gen

prop_lastNShouldSkip0Weights :: AlphaChar -> AlphaChar -> Int -> StdGen -> Property
prop_lastNShouldSkip0Weights (Alpha c1) (Alpha c2) len gen
  = len > 0 ==> lastNShouldSkip0Weights' lastN'
  where lastN' = evalRand (lastN len [c2, c1]) gen

lastNShouldSkip0Weights' :: String -> Property
lastNShouldSkip0Weights' (p:ps:pss:psss) = isCandidate p [pss, ps] .&&.
                                           lastNShouldSkip0Weights' (ps:pss:psss)
lastNShouldSkip0Weights' _ = property True

-- Utility functions
isCandidate :: Char -> String -> Property
isCandidate c (s:ss:_) = hasCandidates f2 ==>
                           isJust $ elem c `liftM` findNextCandidates f2
  where f2 = [s, ss]

isCandidate _ _ = undefined -- This really shouldn't ever happen

hasCandidates :: String -> Bool
hasCandidates s = (not . null) `liftM` findNextCandidates s == Just True

findNextCandidates :: String -> Maybe String
findNextCandidates (c1:c2:_) = map fst `liftM` frequency
  where f2 = [c1, c2]
        frequency = (filter ((/=0) . snd) . zip ['a'..'z']) `liftM` findFrequency f2

findNextCandidates _ = Nothing  -- This really shouldn't ever happen
