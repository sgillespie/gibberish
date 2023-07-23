module Main where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import qualified Test.Elocrypt.TrigraphTest as TrigraphTest
import qualified Test.Elocrypt.UtilsTest as UtilsTest
import qualified Test.ElocryptTest as PasswordTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Unit Tests"
    [ PasswordTest.tests,
      TrigraphTest.tests,
      UtilsTest.tests
    ]
