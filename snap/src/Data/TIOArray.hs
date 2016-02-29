{-# LANGUAGE FlexibleInstances,FlexibleContexts #-}

module Data.TIOArray(TIOArray) where

import Data.IOArray as IA
import Data.IORef as I
import System.IO.Unsafe
import System.TIO

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

instance MArray (TIOArray i e) e TIO where
  getBounds = TIO . getBounds . realArray
  getNumElements = TIO . getNumElements . realArray

  unsafeNewArray_ ix = TIO $ do
    array <- unsafeNewArray_ ix
    ref <- I.newIORef (Just array)
    return $ TIOArray array ref

  unsafeRead i from = TIO $ do
    shadow <- I.readIORef (shadowArray from)
    case shadow of
      Nothing -> unsafeRead i (realArray from)
      Just array -> unsafeRead i array

  unsafeWrite i to v = TIO $ do
    shadow <- I.readIORef (shadowArray from)
    shadow <- case shadow of
      Nothing -> do
        bounds <- getBounds (realArray from)
        newShadow <- unsafeNewArray bounds
        copyArray (realArray from) newShadow
        I.writeIORef (shadowArray from) newShadow
        return newShadow
      Just something -> return something
  unsafeWrite i shadow v

