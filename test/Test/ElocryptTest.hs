{-# LANGUAGE TemplateHaskell #-}
module Test.ElocryptTest where

import Control.Monad
import Control.Monad.Random
import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

import Data.Elocrypt
import Data.Elocrypt.Trigraph
import Test.Elocrypt.Instances

tests :: TestTree
tests = $(testGroupGenerator)

-- |Passwords have the specified length
prop_newPasswordHasLen :: Positive Int -> Bool -> StdGen -> Bool
prop_newPasswordHasLen (Positive len) caps gen
  = length pass == len
  where pass = newPassword len caps gen

-- |Passwords are all lowercase when caps is false
prop_newPasswordIsLower :: Positive Int -> StdGen -> Bool
prop_newPasswordIsLower (Positive len) gen
  = all isLower pass
  where pass = newPassword len False gen

-- |Passwords with caps generates caps
prop_newPasswordHasCaps :: Positive Int -> StdGen -> Property
prop_newPasswordHasCaps (Positive len) gen
  = cover (any isUpper pass) 50 "has caps" $
      any isLower pass
  where pass = newPassword (len + 2) True gen

-- |Third and each successive character is taken from the trigraph
prop_3rdCharHasPositiveFrequency :: Positive Int -> Bool -> StdGen -> Property
prop_3rdCharHasPositiveFrequency (Positive len) caps gen
  = conjoin $ loop pass
  where pass = newPassword (len+2) caps gen
        loop (f:s:t:xs) = thirdCharIsInTrigraph [f, s, t] : loop (s:t:xs)
        loop _          = []

thirdCharIsInTrigraph :: String -> Property
thirdCharIsInTrigraph pass
  = counterexample failMsg $ property (t `elem` candidates)
  where (f:s:t:_) = map toLower pass
        candidates = map fst . filter ((0/=) . snd) $ frequencies
        frequencies = zip ['a'..'z'] .
                      defaultFrequencies .
                      fromJust .
                      findFrequency $ [f, s]

        failMsg = t : " not in [" ++ candidates ++ "]"

-- First 2 characters have total non-zero frequencies
prop_first2HavePositiveFrequencies :: Positive Int -> Bool -> StdGen -> Property
prop_first2HavePositiveFrequencies (Positive len) caps gen
  = counterexample failMsg $ property (sum frequencies > 0)
  where pass = newPassword (len+1) caps gen
        (f:s:_) = map toLower pass
        frequencies = zipWith (curry snd) ['a'..'z'] .
                      fromJust .
                      findFrequency $ [f, s]
        failMsg = "no candidates for '" ++ [f, s] ++ "'"

-- (len . fst) (genPasswords _ x _ _) = x
prop_newPasswordsHasLen
  :: Positive Int
  -> Positive Int
  -> Bool
  -> StdGen
  -> Property
prop_newPasswordsHasLen (Positive len) (Positive num) caps gen
  = counterexample failMsg $ property (length passes == num)
  where passes = newPasswords len num caps gen
        failMsg     = show (length passes) ++ " /= " ++ show num

-- (all ((x==) . length) . fst) (genPasswords x _ _ _) = x
prop_newPasswordsAllHaveLen
  :: Positive Int
  -> Positive Int
  -> Bool
  -> StdGen
  -> Bool
prop_newPasswordsAllHaveLen (Positive len) (Positive num) caps gen
  = all ((len==) . length) passes
  where passes = newPasswords len num caps gen

-- |Given the same generator, newPassword and genPassword generates the 
--  same password.
prop_newPasswordMatchesGenPassword
  :: Positive Int
  -> Bool
  -> StdGen
  -> Property
prop_newPasswordMatchesGenPassword (Positive len) caps gen
  = counterexample failMsg $ property (pass == pass')
  where (pass, _) = genPassword len caps gen
        pass'     = newPassword len caps gen
        failMsg   = show pass ++ " /= " ++ show pass'

-- |Given the same generator, newPasswords and genPasswords genereates
--  the same passwords.
prop_newPasswordsMatchesGenPasswords
  :: Positive Int
  -> Positive Int
  -> Bool
  -> StdGen
  -> Property
prop_newPasswordsMatchesGenPasswords (Positive len) (Positive num) caps gen
  = counterexample failMsg $ property (passes == passes')
  where (passes, _) = genPasswords len num caps gen
        passes'     = newPasswords len num caps gen
        failMsg     = show passes ++ " /= " ++ show passes'

-- |newPassphrase generates n words
prop_newPassphraseHasLen 
  :: Positive Int
  -> Positive Int
  -> Positive Int
  -> StdGen
  -> Property
prop_newPassphraseHasLen (Positive len) (Positive min) (Positive max) gen
  = counterexample failMsg $ property (length words == len)
  where words   = newPassphrase len min max gen
        failMsg = show len ++ " /= length " ++ show words

-- |newPassphrase generates words in the allowed range
prop_newPassphraseWordsHaveLen
  :: Positive Int
  -> Positive Int
  -> Positive Int
  -> StdGen
  -> Bool
prop_newPassphraseWordsHaveLen (Positive len) (Positive min) (Positive max) gen
  = all (\w -> length w >= min && length w <= max') words
  where words = newPassphrase len min max' gen
        max'  = min + max

-- |Given the same generator, genPassphrase and newPassphrase generates
-- the same passphrase
prop_genPassphraseMatchesNewPassphrase
  :: Positive Int
  -> Positive Int
  -> Positive Int
  -> StdGen
  -> Property
prop_genPassphraseMatchesNewPassphrase 
  (Positive len) (Positive min) (Positive max) gen 
  = counterexample failMsg $ property (words == words')
  where (words, _) = genPassphrase len min max' gen
        words'     = newPassphrase len min max' gen
        max'       = min + max
        failMsg    = show words ++ " /= " ++ show words'
