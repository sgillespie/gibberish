module Main where

import Control.Monad
import System.Random

import Elocrypt.Password

main :: IO ()
main = (fst . (flip generate) 6) `liftM` getStdGen >>= putStrLn

