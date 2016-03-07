{-# LANGUAGE Unsafe #-}

module System.TIO.Internal(TIO(..)) where

import Control.Applicative
import Control.Monad

data TIO a = TIO { unsafeRunTIO :: IO a }

instance Functor TIO where
  fmap = liftM

instance Applicative TIO where
  pure = return
  (<*>) = ap

instance Monad TIO where
  return = TIO . return
  m >>= f = TIO $ unsafeRunTIO m >>= (unsafeRunTIO . f)

-- intentionally no instance of MonadIO here, or we
-- would run IO actions not transactionally
