{-|
Module:      Elocrypt.Passowrd
Description: Generate pronouncable, hard-to-guess passwords
Copyright:   (c) Sean Gillespie, 2015
License:     OtherLicense
Maintainer:  Sean Gillespie <sean@mistersg.net>
Stability:   Experimental

Generate easy-to-remember, hard-to-guess passwords
-}
module Data.Elocrypt where

import Data.Elocrypt.Trigraph

import Control.Monad
import Control.Monad.Random hiding (next)
import Data.Bool
import Data.Maybe
import Prelude hiding (min, max)

-- * Random password generators

-- |Generate a password using the generator g, returning the result and the
--  updated generator.
--
--  @
--  -- Generate a password of length 10 using the system generator
--  myGenPassword :: IO (String, StdGen)
--  myGenPassword = genPassword 10 True \`liftM\` getStdGen
--  @
genPassword :: RandomGen g
               => Int  -- ^ password length
               -> Bool -- ^ include capitals?
               -> g    -- ^ random generator
               -> (String, g)
genPassword len = runRand . mkPassword len

-- |Plural version of genPassword.  Generates an infinite list of passwords
--  using the generator g, returning the result and the updated generator.
--
-- @
-- -- Generate 10 passwords of length 10 using the system generator
-- myGenPasswords :: IO ([String], StdGen)
-- myGenPasswords = (\(ls, g) -> (ls, g) `liftM` genPasswords 10 10 True `liftM` getStdGen
-- @
genPasswords :: RandomGen g
                => Int  -- ^ password length
                -> Int  -- ^ number of passwords
                -> Bool -- ^ include capitals?
                -> g    -- ^ random generator
                -> ([String], g)
genPasswords len n = runRand . mkPasswords len n

-- |Generate a password using the generator g, returning the result.
--
--  @
--  -- Generate a password of length 10 using the system generator
--  myNewPassword :: IO String
--  myNewPassword = newPassword 10 True \`liftM\` getStdGen
--  @
newPassword :: RandomGen g
               => Int  -- ^ password length
               -> Bool -- ^ include capitals?
               -> g    -- ^ random generator
               -> String
newPassword len = evalRand . mkPassword len

-- |Plural version of newPassword.  Generates an infinite list of passwords
--  using the generator g, returning the result
--
-- @
-- -- Generate 10 passwords of length 10 using the system generator
-- myNewPasswords :: IO [String]
-- myNewPasswords = genPasswords 10 10 True `liftM` getStdGen
-- @
newPasswords :: RandomGen g
                => Int  -- ^ password length
                -> Int  -- ^ number of passwords
                -> Bool -- ^ include capitals?
                -> g    -- ^ random generator
                -> [String]
newPasswords len n = evalRand . mkPasswords len n

-- |Generate a password using the MonadRandom m. MonadRandom is exposed here
--  for extra control.
--
--  @
--  -- Generate a password of length 10 using the system generator
--  myPassword :: IO String
--  myPassword = evalRand (mkPassword 10 True) \`liftM\` getStdGen
--  @
mkPassword :: MonadRandom m
              => Int  -- ^ password length
              -> Bool -- ^ include capitals?
              -> m String
mkPassword len _ = do
  f2 <- reverse `liftM` first2
  if len > 2
    then reverse `liftM` lastN (len - 2) f2
    else return . reverse . take len $ f2

-- |Plural version of mkPassword.  Generate an infinite list of passwords using
--  the MonadRandom m.  MonadRandom is exposed here for extra control.
--
-- @
-- -- Generate an list of length 20 with passwords of length 10 using the system generator
-- myMkPasswords :: IO [String]
-- myMkPasswords = evalRand (mkPasswords 10 20 True) \`liftM\` getStdGen
-- @
mkPasswords :: MonadRandom m
               => Int  -- ^ password length
               -> Int  -- ^ number of passwords
               -> Bool -- ^ include capitals?
               -> m [String]
mkPasswords len n = replicateM n . mkPassword len

-- * Random passphrase generators

-- |Generate a passphrase using the generator g, returning the result and the
--  updated generator.
--
--  @
--  -- Generate a passphrase of 10 words, each having a length between 6 and 12,
--  -- using the system generator
--  myGenPassphrase :: IO (String, StdGen)
--  myGenPassphrase = genPassword 10 True \`liftM\` getStdGen
--  @
genPassphrase :: RandomGen g
              => Int  -- ^ number of words
              -> Int  -- ^ minimum word length
              -> Int  -- ^ maximum word length
              -> g    -- ^ random generator
              -> ([String], g)
genPassphrase n min = runRand . mkPassphrase n min

-- |Generate a passphrase using the generator g, returning the result.
--
--  @
--  -- Generate a passphrase of 10 words, each having a length between 6 an 12,
--  -- using the system generator.
--  myNewPassphrase :: IO String
--  myNewPassphrase = newPassphrase 10 6 12 \`liftM\` getStdGen
--  @
newPassphrase :: RandomGen g
               => Int  -- ^ number of words
               -> Int  -- ^ minimum word length
               -> Int  -- ^ maximum word length
               -> g    -- ^ random generator
               -> [String]
newPassphrase n min = evalRand . mkPassphrase n min

-- |Generate a finite number of words of random length (between @min@ and @max@ chars)
--  using the MonadRandom m. MonadRandom is exposed here for extra control.
--
--  @
--  -- Generate a passphrase of 10 words, each having a length between 6 and 12.
--  myPassphrase :: IO String
--  myPassphrase = evalRand (mkPassphrase 10 6 12) \`liftM\` getStdGen
--  @
mkPassphrase :: MonadRandom m
             => Int  -- ^ number of words
             -> Int  -- ^ minimum word length
             -> Int  -- ^ maximum word length
             -> m [String]
mkPassphrase n min max = replicateM n $
  getRandomR (min, max) >>= flip mkPassword False

-- * Internal

-- |The alphabet we sample random values from
alphabet :: String
alphabet = ['a'..'z']

-- |Generate two random characters. Uses 'Elocrypt.Trigraph.trigragh'
--  to generate a weighted list.
first2 :: MonadRandom m => m String
first2 = fromList (map toWeight frequencies)
  where toWeight (s, w) = (s, sum w)

-- |Generate a random character based on the previous two characters and
--  their 'Elocrypt.Trigraph.trigraph'
next :: MonadRandom m => String -> m Char
next (x:xs:_) = fromList .
                zip ['a'..'z'] .
                defaultFreqs .
                fromJust .
                findFrequency $ [xs, x]

  -- Fix frequencies if they are all 0, since MonadRandom prohibits this.
  -- Use all 1s in this case to give every item an equal weight.
  where defaultFreqs :: [Rational] -> [Rational]
        defaultFreqs f = bool (replicate 26 1) f (any (>0) f)

-- This shouldn't ever happen
next _ = undefined

-- |Generate the last n characters using previous two characters
--  and their 'Elocrypt.Trigraph.trigraph'
lastN :: MonadRandom m => Int -> String -> m String
lastN 0 ls   = return ls
lastN len ls = next ls >>= lastN (len - 1) . (:ls)

