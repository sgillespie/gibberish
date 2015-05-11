module Elocrypt.Password where

import Elocrypt.Trigraph

import Control.Monad
import Control.Monad.Random hiding (next)
import Data.Maybe
import System.Random hiding (next)

generate :: RandomGen g => g -> Int -> (String, g)
generate g len = runRand (mkPassword len) g

mkPassword :: MonadRandom m => Int -> m String
mkPassword len = reverse `liftM` first2 >>= (flip lastN) (len - 2) >>= return . reverse

first2 :: MonadRandom m => m String
first2 = sequence . take 2 . repeat . uniform $ alphabet

lastN :: MonadRandom m => String -> Int -> m String
lastN ls 0 = return ls
lastN ls len = next ls >>= (flip lastN) (len - 1) . (flip (:)) ls

next :: MonadRandom m => String -> m Char
next (x:xs:_) = fromList . zip ['a'..'z'] . fromJust . findFrequency $ [xs,x]

alphabet :: [Char]
alphabet = ['a'..'z']
