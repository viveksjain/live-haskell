module Data.TIORef(TIORef, readIORef, writeIORef, saveIORef) where

import System.TIO
import qualified Data.IORef as I

data TIORef a = TIORef { realRef :: I.IORef a, shadowRef :: I.IORef (Maybe a) }

concretize :: TIORef a -> TIO ()
concretize ref = TIO $ do
  shadow <- I.readIORef $ shadowRef ref
  case shadow of
    Maybe _ -> return ()
    Nothing -> I.readIORef (realRef ref) >>= I.writeIORef (shadowRef ref) . Just

readIORef :: TIORef a -> TIO a
readIORef ref = do
  concretize
  TIO $ I.readIORef (shadowRef ref)

writeIORef :: TIORef a -> a -> TIO ()
writeIORef ref v = TIO $ do
  I.writeIORef (shadowRef ref) v

saveIORef :: TIORef a -> TIO ()
saveIORef ref = TIO $ do
  shadow <- I.readIORef $ shadowRef ref
  case shadow of
    Nothing -> return ()
    Maybe v -> do
      I.writeIORef (realRef ref) v
      I.writeIORef (shadowRef ref) Nothing
