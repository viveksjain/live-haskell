{-# LANGUAGE Trustworthy #-}

module System.TIO(TIO,
                  runTIO,
                  openFile,
                  hClose,
                  withFile,
                  readFile,
                  writeFile,
                  appendFile,
                  hGetContents,
                  hPutStr,
                  hFileSize,
                  hSetFileSize,
                  hIsEOF) where

import Prelude hiding (readFile, writeFile, appendFile)

import Control.Applicative
import Control.Monad
import Control.Exception

import System.TIO.Internal

import System.FilePath((</>))
import System.IO(FilePath, IOMode(..))
import System.IO.Unsafe(unsafePerformIO)

import System.Posix.Directory(getWorkingDirectory, createDirectory)
import System.Posix.Types(FileMode)
import System.Posix.Files(fileExist)
import System.Posix.Process(getProcessID)
import Foreign.ForeignPtr(mallocForeignPtrBytes,withForeignPtr)

import qualified System.IO as S

runTIO :: TIO a -> IO a
runTIO = unsafeRunTIO

sandboxPath :: FilePath
sandboxPath = unsafePerformIO $ do
  dir <- getWorkingDirectory
  pid <- getProcessID
  let path = dir </> (".sandbox-tio-" ++ show pid)
  try (createDirectory path 0600) :: IO (Either SomeException ())
  return path

data Handle = Handle !FilePath !S.Handle deriving (Eq, Show)

copyFile :: FilePath -> FilePath -> IO ()
copyFile from to = do
  buffer <- mallocForeignPtrBytes 8192 -- two memory blocks
  let closeReadFile file = case file of
        Left error -> return ()
        Right handle -> S.hClose handle
      readAction :: IO (Either IOError S.Handle)
      readAction = try $ S.openBinaryFile from ReadMode
  bracket readAction closeReadFile $ \fromFile -> do
    case fromFile of
      Left error -> return ()
      Right fromH -> do
        bracket (S.openBinaryFile to WriteMode) S.hClose $ \toH -> do
          let loop = withForeignPtr buffer $ \buffer -> do
                read <- S.hGetBuf fromH buffer 8192
                if read > 0
                  then S.hPutBuf toH buffer read >> loop
                  else return ()
          loop


openFile :: FilePath -> IOMode -> TIO Handle
openFile path mode = TIO $ do
  let insandbox = sandboxPath </> path
  putStrLn $ "insandbox path " ++ insandbox
  exist <- fileExist insandbox
  when (not exist) (copyFile path insandbox)
  handle <- S.openFile insandbox mode
  return $ Handle path handle

hClose :: Handle -> TIO ()
hClose (Handle _ handle) = TIO $ S.hClose handle

withFile :: FilePath -> IOMode -> (Handle -> TIO a) -> TIO a
withFile path mode f = TIO $ bracket (runTIO $ openFile path mode) (runTIO . hClose) (runTIO . f)

readFile :: FilePath -> TIO String
readFile path = openFile path ReadMode >>= hGetContents
writeFile :: FilePath -> String -> TIO ()
writeFile path str = withFile path WriteMode $ flip hPutStr str
appendFile :: FilePath -> String -> TIO ()
appendFile path str = withFile path AppendMode $ flip hPutStr str

hGetContents :: Handle -> TIO String
hGetContents (Handle _ handle) = TIO $ S.hGetContents handle

hPutStr :: Handle -> String -> TIO ()
hPutStr (Handle _ handle) str = TIO $ S.hPutStr handle str

hFileSize :: Handle -> TIO Integer
hFileSize (Handle _ handle) = TIO $ S.hFileSize handle

hSetFileSize :: Handle -> Integer -> TIO ()
hSetFileSize (Handle _ handle) size = TIO $ S.hSetFileSize handle size

hIsEOF :: Handle -> TIO Bool
hIsEOF (Handle _ handle) = TIO $ S.hIsEOF handle

