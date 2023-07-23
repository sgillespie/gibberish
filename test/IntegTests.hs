module Main where

import Control.Monad
import Data.Functor
import Data.List
import Data.Maybe
import Text.Printf

import Test.Proctest
import Test.Proctest.Assertions
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.Tasty hiding (Timeout)
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)
import Test.Tasty.TH

import qualified IntegTest.Elocrypt.PassphraseTest as PassphraseTest
import qualified IntegTest.Elocrypt.PasswordTest as PasswordTest

main :: IO ()
main = defaultMain (options tests)

options :: TestTree -> TestTree
options = localOption (QuickCheckTests 10)

tests :: TestTree
tests =
  testGroup
    "Integration Tests"
    [ PasswordTest.tests,
      PassphraseTest.tests
    ]
