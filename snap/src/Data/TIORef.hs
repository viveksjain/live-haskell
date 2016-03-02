{-# LANGUAGE Trustworthy #-}

module Data.TIORef(TIORef, readIORef, writeIORef, saveIORef) where

import System.TIO.Internal
import qualified Data.IORef as I

data TIORef a = TIORef { realRef :: I.IORef a, shadowRef :: I.IORef (Maybe a) }

concretize :: TIORef a -> IO ()
concretize ref = do
  shadow <- I.readIORef $ shadowRef ref
  case shadow of
    Just _ -> return ()
    Nothing -> I.readIORef (realRef ref) >>= I.writeIORef (shadowRef ref) . Just

readIORef :: TIORef a -> TIO a
readIORef ref = TIO $ do
  concretize ref
  read <- I.readIORef (shadowRef ref)
  case read of
    Just v -> return v
    Nothing -> fail "what?"

writeIORef :: TIORef a -> a -> TIO ()
writeIORef ref v = TIO $ do
  I.writeIORef (shadowRef ref) (Just v)

saveIORef :: TIORef a -> TIO ()
saveIORef ref = TIO $ do
  shadow <- I.readIORef $ shadowRef ref
  case shadow of
    Nothing -> return ()
    Just v -> do
      I.writeIORef (realRef ref) v
      I.writeIORef (shadowRef ref) Nothing
