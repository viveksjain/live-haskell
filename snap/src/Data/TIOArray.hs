{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}

module Data.TIOArray(TIOArray) where

import Control.Monad

import Data.Array.Base
import Data.Array.IO as IA
import Data.IORef as I
import System.IO.Unsafe
import System.TIO.Internal

data TIOArray i e = TIOArray { realArray :: IA.IOArray i e,
                               shadowArray :: I.IORef (Maybe (IA.IOArray i e)) }

extractReal :: TIOArray i e -> IA.IOArray i e
extractReal from =
  let shadow = unsafePerformIO $ I.readIORef (shadowArray from)
  in
   case shadow of
     Nothing -> realArray from
     Just array -> array

instance Eq (TIOArray i e) where
  l == r = extractReal l == extractReal r

instance MArray TIOArray e TIO where
  getBounds = TIO . getBounds . realArray
  getNumElements = TIO . getNumElements . realArray

  unsafeNewArray_ ix = TIO $ do
    array <- unsafeNewArray_ ix
    ref <- I.newIORef (Just array)
    return $ TIOArray array ref

  unsafeRead from i = TIO $ do
    shadow <- I.readIORef (shadowArray from)
    case shadow of
      Nothing -> unsafeRead (realArray from) i
      Just array -> unsafeRead array i

  unsafeWrite to i v = TIO $ do
    shadow <- I.readIORef (shadowArray to)
    shadow <- case shadow of
      Nothing -> do
        bounds <- getBounds (realArray to)
        newShadow <- unsafeNewArray_ bounds
        copyArray (realArray to) newShadow
        I.writeIORef (shadowArray to) (Just newShadow)
        return newShadow
      Just something -> return something
    unsafeWrite shadow i v

copyArray :: (Ix i, Monad m, MArray a e m) => a i e -> a i e -> m ()
copyArray from to = do
  (low, high) <- getBounds from
  (lowT, highT) <- getBounds to
  when ((low, high) /= (lowT, highT)) $ fail "arrays have different size"
  forM_ (range (low, high)) $ \i -> do
    e <- readArray from i
    writeArray to i e
