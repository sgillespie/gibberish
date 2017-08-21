module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments = [ 
  "src",
  "test"
  ]

main :: IO ()
main = hlint arguments >>= main'
  where main' [] = exitSuccess
main' _ = exitFailure
