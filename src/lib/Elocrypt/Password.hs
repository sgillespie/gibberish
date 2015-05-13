{-|
Module:      Elocrypt.Passowrd
Description: Generate pronouncable, hard-to-guess passwords
Copyright:   (c) Sean Gillespie, 2015
License:     OtherLicense
Maintainer:  Sean Gillespie <sean@mistersg.net>
Stability:   Experimental

Generate easy-to-remember, hard-to-guess passwords
-}
module Elocrypt.Password where

import Elocrypt.Trigraph

import Control.Monad
import Control.Monad.Random hiding (next)
import Data.Maybe
import System.Random hiding (next)

-- * Random password generators

-- |Generate a password using the generator g, returning the result and the
--  updated generator.
--
--  @
--  -- Generate a password of length 10 using the system generator
--  myGenPassword :: IO (String, StdGen)
--  myGenPassword = genPassword 10 \`liftM\` getStdGen
--  @
genPassword :: RandomGen g
               => Int -- ^ password length
               -> g   -- ^ random generator
               -> (String, g)
genPassword len gen = runRand (mkPassword len) gen

-- |Generate a password using the generator g, returning the result.
--
--  @
--  -- Generate a password of length 10 using the system generator
--  myNewPassword :: IO String
--  myNewPassword = newPassword 10 \`liftM\` getStdGen
--  @
newPassword :: RandomGen g
               => Int -- ^ password length
               -> g   -- ^  random generator
               -> String
newPassword len gen = evalRand (mkPassword len) gen

-- |Generate a password using the MonadRandom m. MonadRandom is exposed here
--  for extra control.
--
--  @
--  -- Generate an infinite list of passwords of length 10 using the system generator
--  myPasswords :: IO [String]
--  myPasswords = evalRand (sequence . repeat . mkPassword $ 10) \`liftM\` getStdGen
--  @
mkPassword :: MonadRandom m
              => Int
              -> m String
mkPassword len = reverse `liftM` first2 >>= (flip lastN) (len - 2) >>= return . reverse

-- |The alphabet we sample random values from
alphabet :: [Char]
alphabet = ['a'..'z']

-- * Internal

-- |Generate two random characters
first2 :: MonadRandom m => m String
first2 = sequence . take 2 . repeat . uniform $ alphabet

-- |Generate a random character based on the previous two characters and
--  their 'Elocrypt.Trigraph.trigraph'
next :: MonadRandom m => String -> m Char
next (x:xs:_) = fromList . zip ['a'..'z'] . fromJust . findFrequency $ [xs,x]

-- |Generate the last n characters using previous two characters
--  and their 'Elocrypt.Trigraph.trigraph'
lastN :: MonadRandom m => String -> Int -> m String
lastN ls 0 = return ls
lastN ls len = next ls >>= (flip lastN) (len - 1) . (flip (:)) ls
