module Main where

import Control.Monad
import System.Random

import Elocrypt.Password

main :: IO ()
main = (fst . generate) `liftM` getStdGen >>= putStrLn
