module Main where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import qualified Elocrypt.PasswordTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [Elocrypt.PasswordTest.tests]
