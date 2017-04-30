module Main where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import qualified Test.ElocryptTest as PasswordTest
import qualified Test.Elocrypt.TrigraphTest as TrigraphTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit Tests" [PasswordTest.tests,
                                TrigraphTest.tests]
