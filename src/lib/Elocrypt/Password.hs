module Elocrypt.Password where

import Control.Monad.Random
import System.Random

generate :: RandomGen g => g -> Int -> (String, g)
generate gen len = runRand m gen
  where weights = zip alphabet (repeat 1)
        m = sequence . take len . repeat . fromList $ weights

alphabet :: [Char]
alphabet = ['a'..'z']
