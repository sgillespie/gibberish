{-# LANGUAGE TemplateHaskell #-}
module Elocrypt.PasswordTest where

import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

import Elocrypt.Password

tests = $(testGroupGenerator)

prop_generate = generate == "changeme!"
