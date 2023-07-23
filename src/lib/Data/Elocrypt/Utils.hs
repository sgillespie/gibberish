module Data.Elocrypt.Utils where

import Data.Char (isAlphaNum, isSpace)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Ratio

import Control.Monad.Random (MonadRandom (), fromList)

-- | A mapping from letters to numbers that look like them
numeralConversions =
  M.fromList
    [ ('o', ['0']),
      ('l', ['1']),
      ('z', ['2']),
      ('e', ['3']),
      ('a', ['4']),
      ('s', ['5']),
      ('g', ['6', '9']),
      ('t', ['7']),
      ('b', ['8'])
    ]

-- | A mapping from letters to symbols that look like them
symbolConversions =
  M.fromList
    [ ('a', ['@']),
      ('l', ['!']),
      ('s', ['$'])
    ]

-- | Map a letter to one or more digits, if possible
toDigit :: Char -> String
toDigit c = fromMaybe [c] (numeralConversions M.!? c)

-- | Map a letter to one or more symbols, if possible
toSymbol :: Char -> String
toSymbol c = fromMaybe [c] (symbolConversions M.!? c)

-- | Selects special characters
isSymbol :: Char -> Bool
isSymbol c = not (isAlphaNum c || isSpace c)

-- | Randomly update characters at the specified probability
updateR
  :: MonadRandom m
  => (Char -> m Char)
  -> Rational
  -> String
  -> m String
updateR f prob = mapM f'
  where
    f' ch = do
      ch' <- f ch
      fromList
        [ (ch, toRational $ denominator prob),
          (ch', toRational $ numerator prob)
        ]

-- | Update character at position pos
update1
  :: Monad m
  => (Char -> m Char)
  -- ^ Update function
  -> String
  -- ^ the string to update
  -> Int
  -- ^ the position to update
  -> m String
update1 _ "" _ = return ""
update1 f s pos = (\ch' -> prefix ++ ch' : suffix) <$> f ch
  where
    (prefix, ch : suffix) = splitAt pos s
