module Data.Gibberish.Utils
  ( numeralConversions,
    symbolConversions,
    update1,
    updateR,
    findIndices,
    textTraverse,
  ) where

import Control.Monad.Random (MonadRandom (), fromList)
import Data.Map (Map ())
import Data.Map qualified as Map
import Data.Ratio (denominator, numerator)
import Data.Text (Text ())
import Data.Text qualified as Text

-- | A mapping from letters to numbers that look like them
numeralConversions :: Map Char [Char]
numeralConversions =
  Map.fromList
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
symbolConversions :: Map Char [Char]
symbolConversions =
  Map.fromList
    [ ('a', ['@']),
      ('l', ['!']),
      ('s', ['$'])
    ]

update1 :: Monad m => (Char -> m Char) -> Text -> Int -> m Text
update1 f t pos =
  case Text.splitAt pos t of
    (prefix, suffix) ->
      case Text.uncons suffix of
        Nothing -> pure t
        Just (ch, suffix') -> do
          ch' <- f ch
          pure $ prefix `Text.append` (ch' `Text.cons` suffix')

updateR :: MonadRandom m => (Char -> m Char) -> Rational -> Text -> m Text
updateR f prob = textTraverse updateR'
  where
    updateR' ch = do
      ch' <- f ch
      fromList
        [ (ch, toRational $ denominator prob),
          (ch', toRational $ numerator prob)
        ]

-- | /O(n)/ The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Char -> Bool) -> Text -> [Int]
findIndices p = loop 0
  where
    loop !n !qs = case Text.findIndex p qs of
      Just !i ->
        let !j = n + i
        in j : loop (j + 1) (Text.drop (i + 1) qs)
      Nothing -> []
{-# INLINE [1] findIndices #-}

textTraverse :: Monad m => (Char -> m Char) -> Text -> m Text
textTraverse f = Text.foldr folder (pure Text.empty)
  where
    folder c accum = do
      accum' <- accum
      c' <- f c
      pure $ Text.cons c' accum'
