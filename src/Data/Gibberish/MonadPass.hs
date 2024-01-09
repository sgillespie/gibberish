module Data.Gibberish.MonadPass
  ( PassT (..),
    runPassT,
    evalPassT,
    usingPassT,
  ) where

import Data.Gibberish.Types (GenPassOptions ())

import Control.Monad.Random.Class (MonadRandom ())
import Control.Monad.Reader (MonadReader (), ReaderT (), runReaderT)
import Control.Monad.Trans.Random (RandT (), runRandT)

-- | Password/Passphrase generation monad
newtype PassT g m a = PassT {unPass :: ReaderT GenPassOptions (RandT g m) a}
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadFail,
      MonadRandom,
      MonadReader GenPassOptions
    )

-- | Run a generation computation with the given options and initial generator
runPassT :: PassT g m a -> GenPassOptions -> g -> m (a, g)
runPassT act = runRandT . runReaderT (unPass act)

-- | Evaluate a generation computation with the given options and initial
--   generator, discarding the final generator
evalPassT :: Functor m => PassT g m a -> GenPassOptions -> g -> m a
evalPassT act opts gen = fst <$> runPassT act opts gen

-- | Like @runGenT@, but the computation is the last argument
usingPassT :: GenPassOptions -> g -> PassT g m a -> m (a, g)
usingPassT opts gen act = runPassT act opts gen
