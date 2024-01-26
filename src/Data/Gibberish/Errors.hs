module Data.Gibberish.Errors
  ( GibberishErr (..),
    isTrigraphNotFound,
    isImpossibleError,
  ) where

import Control.Exception (Exception ())
import Data.Typeable (Typeable ())

-- | Exceptions that can occur at runtime
data GibberishErr
  = TrigraphNotFound FilePath
  | ImpossibleError
  deriving stock (Eq, Typeable)

instance Exception GibberishErr

instance Show GibberishErr where
  show (TrigraphNotFound path) = "Trigraph file " <> show path <> " does not exist!"
  show ImpossibleError = "The impossible happened! Please file a bug report."

isTrigraphNotFound :: GibberishErr -> Bool
isTrigraphNotFound (TrigraphNotFound _) = True
isTrigraphNotFound _ = False

isImpossibleError :: GibberishErr -> Bool
isImpossibleError ImpossibleError = True
isImpossibleError _ = False
