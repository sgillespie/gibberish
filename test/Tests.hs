module Main where

import Test.Elocrypt.TrigraphTest qualified as TrigraphTest
import Test.Elocrypt.TypesTest qualified as TypesTest
import Test.Elocrypt.UtilsTest qualified as UtilsTest
import Test.ElocryptTest qualified as PasswordTest

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Unit Tests"
    [ PasswordTest.tests,
      TrigraphTest.tests,
      UtilsTest.tests,
      TypesTest.tests
    ]
