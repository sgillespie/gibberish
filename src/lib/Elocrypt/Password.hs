module Elocrypt.Password where

import Control.Monad.Random
import System.Random

generate :: RandomGen g => g -> (String, g)
generate gen = runRand m gen
  where weights = zip alphabet (repeat 1)
        m = sequence . take 6 . repeat . fromList $ weights

alphabet :: [Char]
alphabet = ['a'..'z']
