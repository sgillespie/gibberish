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
import Test.Elocrypt.QuickCheck
import Test.Elocrypt.Instances

tests :: TestTree
tests = $(testGroupGenerator)

-- |Passwords have the specified length
prop_newPasswordHasLen :: Positive Int -> GenOptions -> StdGen -> Bool
prop_newPasswordHasLen (Positive len) opts gen = length pass == len
  where pass = newPassword len opts gen

-- |Passwords are all lowercase when caps is false
prop_newPasswordIsLower :: Positive Int -> StdGen -> Bool
prop_newPasswordIsLower (Positive len) gen = all isLower pass
  where
    pass = newPassword len opts gen
    opts = genOptions { genCapitals = False }

-- |Passwords with caps have at least one capital
prop_newPasswordHasCaps :: Positive Int -> StdGen -> Bool
prop_newPasswordHasCaps (Positive len) gen = any isUpper pass
  where
    pass = newPassword (len + 2) opts gen
    opts = genOptions { genCapitals = True }

-- |Most passwords have more than one capital
prop_newPasswordHasMultipleCaps :: Positive Int -> StdGen -> Property
prop_newPasswordHasMultipleCaps (Positive len) gen =
  checkCoverage $
    cover 50 (length (filter isUpper pass) > 1) "has multiple caps" True
  where
    pass = newPassword (len + 2) opts gen
    opts = genOptions { genCapitals = True }

-- |Passwords are all letters when numbers is false
prop_newPasswordIsAlpha :: Positive Int -> StdGen -> Bool
prop_newPasswordIsAlpha (Positive len) gen = all isAlpha pass
  where
    pass = newPassword len opts gen
    opts = genOptions { genDigits = False }

-- |Most passwords have at least one number
prop_newPasswordHasDigits :: Positive Int -> StdGen -> Property
prop_newPasswordHasDigits (Positive len) gen =
  checkCoverage $
    cover 50 (any isDigit pass) "has digits" True
  where
    pass = newPassword (len + 2) opts gen
    opts = genOptions { genDigits = True }
  

-- |Third and each successive character is taken from the trigraph
prop_3rdCharHasPositiveFrequency
  :: Positive Int -> GenOptions -> StdGen -> Property
prop_3rdCharHasPositiveFrequency (Positive len) opts gen = conjoin $ loop pass
  where
    pass = newPassword (len + 2) opts gen
    loop (f : s : t : xs) = thirdCharIsInTrigraph [f, s, t] : loop (s : t : xs)
    loop _ = []

thirdCharIsInTrigraph :: String -> Property
thirdCharIsInTrigraph pass = counterexample failMsg
  $ property (t `elem` candidates)
  where
    (f : s : t : _) = map toLower pass
    candidates = map fst . filter ((0 /=) . snd) $ frequencies
    frequencies =
      zip ['a' .. 'z'] . defaultFrequencies . fromJust . findFrequency $ [f, s]

    failMsg = t : " not in [" ++ candidates ++ "]"

-- First 2 characters have total non-zero frequencies
prop_first2HavePositiveFrequencies
  :: Positive Int -> GenOptions -> StdGen -> Property
prop_first2HavePositiveFrequencies (Positive len) opts gen =
  counterexample failMsg $ property (sum frequencies > 0)
  where
    pass = newPassword (len + 1) opts gen
    (f : s : _) = map toLower pass
    frequencies =
      zipWith (curry snd) ['a' .. 'z'] . fromJust . findFrequency $ [f, s]
    failMsg = "no candidates for '" ++ [f, s] ++ "'"

-- (len . fst) (genPasswords _ x _ _) = x
prop_newPasswordsHasLen
  :: Positive Int -> Positive Int -> GenOptions -> StdGen -> Property
prop_newPasswordsHasLen (Positive len) (Positive num) opts gen =
  counterexample failMsg $ property (length passes == num)
  where
    passes = newPasswords len num opts gen
    failMsg = show (length passes) ++ " /= " ++ show num

-- (all ((x==) . length) . fst) (genPasswords x _ _ _) = x
prop_newPasswordsAllHaveLen
  :: Positive Int -> Positive Int -> GenOptions -> StdGen -> Bool
prop_newPasswordsAllHaveLen (Positive len) (Positive num) opts gen = all
  ((len ==) . length)
  passes
  where passes = newPasswords len num opts gen

-- |Given the same generator, newPassword and genPassword generates the 
--  same password.
prop_newPasswordMatchesGenPassword
  :: Positive Int -> GenOptions -> StdGen -> Property
prop_newPasswordMatchesGenPassword (Positive len) opts gen =
  counterexample failMsg $ property (pass == pass')
  where
    (pass, _) = genPassword len opts gen
    pass' = newPassword len opts gen
    failMsg = show pass ++ " /= " ++ show pass'

-- |Given the same generator, newPasswords and genPasswords genereates
--  the same passwords.
prop_newPasswordsMatchesGenPasswords
  :: Positive Int -> Positive Int -> GenOptions -> StdGen -> Property
prop_newPasswordsMatchesGenPasswords (Positive len) (Positive num) opts gen =
  counterexample failMsg $ property (passes == passes')
  where
    (passes, _) = genPasswords len num opts gen
    passes' = newPasswords len num opts gen
    failMsg = show passes ++ " /= " ++ show passes'

-- |newPassphrase generates n words
prop_newPassphraseHasLen
  :: Positive Int
  -> Positive Int
  -> Positive Int
  -> GenOptions
  -> StdGen
  -> Property
prop_newPassphraseHasLen (Positive len) (Positive min) (Positive max) opts gen
  = counterexample failMsg $ property (length words == len)
  where
    words = newPassphrase len min max opts gen
    failMsg = show len ++ " /= length " ++ show words

-- |newPassphrase generates words in the allowed range
prop_newPassphraseWordsHaveLen
  :: Positive Int
  -> Positive Int
  -> Positive Int
  -> GenOptions
  -> StdGen
  -> Bool
prop_newPassphraseWordsHaveLen (Positive len) (Positive min) (Positive max) opts gen
  = all (\w -> length w >= min && length w <= max') words
  where
    words = newPassphrase len min max' opts gen
    max' = min + max

-- |Given the same generator, genPassphrase and newPassphrase generates
-- the same passphrase
prop_genPassphraseMatchesNewPassphrase
  :: Positive Int
  -> Positive Int
  -> Positive Int
  -> GenOptions
  -> StdGen
  -> Property
prop_genPassphraseMatchesNewPassphrase (Positive len) (Positive min) (Positive max) opts gen
  = counterexample failMsg $ property (words == words')
  where
    (words, _) = genPassphrase len min max' opts gen
    words' = newPassphrase len min max' opts gen
    max' = min + max
    failMsg = show words ++ " /= " ++ show words'
