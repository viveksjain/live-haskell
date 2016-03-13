{-# LANGUAGE Trustworthy #-}

module System.TIO(TIO,
                  runTIO,
                  wipeSandbox,
                  IOMode(..),
                  BufferMode(..),
                  SeekMode(..),
                  HandlePosn,
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
                  hIsEOF,
                  isEOF,
                  hSetBuffering,
                  hGetBuffering,
                  hFlush,
                  hGetPosn,
                  hSetPosn,
                  hSeek,
                  hTell,
                  hWaitForInput,
                  hReady,
                  hGetChar,
                  hGetLine,
                  hLookAhead,
                  hPutChar,
                  hPutStrLn,
                  hPrint,
                  putChar,
                  putStr,
                  putStrLn,
                  print,
                  getChar,
                  getLine,
                  getContents,
                  interact,
                  readIO,
                  readLn,
                  stdin,
                  stdout,
                  stderr) where

import Prelude hiding (readFile, writeFile, appendFile, getContents,
                       getLine, putStr, putStrLn, readIO,
                       putChar, print, getChar, interact, readLn)

import Control.Applicative
import Control.Monad
import Control.Exception hiding (handle)

import System.TIO.Internal

import System.FilePath((</>), takeDirectory)
import System.IO(FilePath, IOMode(..), BufferMode(..), SeekMode(..), HandlePosn)
import System.IO.Unsafe(unsafePerformIO)
import System.IO.Error(isAlreadyExistsError, isDoesNotExistError)

import System.Posix.Directory(getWorkingDirectory, createDirectory)
import System.Directory(removeDirectoryRecursive)
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
  tryJust (guard . isAlreadyExistsError) (createDirectory path 0o700)
  return path

wipeSandbox :: IO ()
wipeSandbox = void $ do
  removeDirectoryRecursive sandboxPath
  tryJust (guard . isAlreadyExistsError) (createDirectory sandboxPath 0o700)

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

ensureDirectory :: FilePath -> IO ()
ensureDirectory "/" = return ()
ensureDirectory file = do
  let dir = takeDirectory file
  --putStrLn $ "creating " ++ dir
  let loop = do
        res <- tryJust maybeInterestingError (createDirectory dir 0o700)
        --putStrLn $ "result " ++ show res
        case (res :: Either IOError ()) of
          Left e ->
            if isDoesNotExistError e
            then ensureDirectory dir >> loop
            else if isAlreadyExistsError e
                 then return ()
                 else undefined
          Right _ -> return ()
  loop
  where
    isInterestingError = pure (||) <*> isDoesNotExistError <*> isAlreadyExistsError
    maybeInterestingError e = if isInterestingError e then Just e else Nothing

openFile :: FilePath -> IOMode -> TIO Handle
openFile path mode = TIO $ do
  let insandbox = case path of
        '/':_ -> sandboxPath </> tail path
        _ -> sandboxPath </> path
  --putStrLn $ "insandbox path " ++ insandbox
  exist <- fileExist insandbox
  when (not exist) $ do
    ensureDirectory insandbox
    case mode of
      WriteMode -> return ()
      _ -> copyFile path insandbox
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

hPutStrLn :: Handle -> String -> TIO ()
hPutStrLn (Handle _ handle) str = TIO $ S.hPutStrLn handle str

hPutChar :: Handle -> Char -> TIO ()
hPutChar (Handle _ handle) ch = TIO $ S.hPutChar handle ch

hPrint :: Show a => Handle -> a -> TIO ()
hPrint handle = hPutStrLn handle . show

hFileSize :: Handle -> TIO Integer
hFileSize (Handle _ handle) = TIO $ S.hFileSize handle

hSetFileSize :: Handle -> Integer -> TIO ()
hSetFileSize (Handle _ handle) size = TIO $ S.hSetFileSize handle size

hIsEOF :: Handle -> TIO Bool
hIsEOF (Handle _ handle) = TIO $ S.hIsEOF handle

isEOF :: TIO Bool
isEOF = hIsEOF stdin

hSetBuffering :: Handle -> BufferMode -> TIO ()
hSetBuffering (Handle _ handle) mode = TIO $ S.hSetBuffering handle mode

hGetBuffering :: Handle -> TIO BufferMode
hGetBuffering (Handle _ handle) = TIO $ S.hGetBuffering handle

hFlush :: Handle -> TIO ()
hFlush (Handle _ handle) = TIO $ S.hFlush handle

hGetPosn :: Handle -> TIO HandlePosn
hGetPosn (Handle _ handle) = TIO $ S.hGetPosn handle

hSetPosn :: HandlePosn -> TIO ()
hSetPosn = TIO . S.hSetPosn

hSeek :: Handle -> SeekMode -> Integer -> TIO ()
hSeek (Handle _ handle) mode pos = TIO $ S.hSeek handle mode pos

hTell :: Handle -> TIO Integer
hTell (Handle _ handle) = TIO $ S.hTell handle

hWaitForInput :: Handle -> Int -> TIO Bool
hWaitForInput (Handle _ handle) timeout = TIO $ S.hWaitForInput handle timeout

hReady :: Handle -> TIO Bool
hReady handle = hWaitForInput handle 0

hGetChar :: Handle -> TIO Char
hGetChar (Handle _ handle) = TIO $ S.hGetChar handle

hGetLine :: Handle -> TIO String
hGetLine (Handle _ handle) = TIO $ S.hGetLine handle

hLookAhead :: Handle -> TIO Char
hLookAhead (Handle _ handle) = TIO $ S.hLookAhead handle

stdin :: Handle
stdin = unsafePerformIO $ do
  stdinHandle <- S.openFile "/dev/null" ReadMode
  return $ Handle "<stdin>" stdinHandle

stdout :: Handle
stdout = Handle "<stdout>" S.stdout

stderr :: Handle
stderr = unsafePerformIO $ do
  stderrHandle <- S.openFile (sandboxPath </> "error.log") WriteMode
  return $ Handle "<stdin>" stderrHandle

interact :: (String -> String) -> TIO ()
interact f = getContents >>= putStr . f

putChar :: Char -> TIO ()
putChar = hPutChar stdout

putStr :: String -> TIO ()
putStr = hPutStr stdout

putStrLn :: String -> TIO ()
putStrLn = hPutStrLn stdout

print :: Show a => a -> TIO ()
print = hPrint stdout

getChar :: TIO Char
getChar = hGetChar stdin

getLine :: TIO String
getLine = hGetLine stdin

getContents :: TIO String
getContents = hGetContents stdin

readIO :: Read a => String -> TIO a
readIO = TIO . S.readIO

readLn :: Read a => TIO a
readLn = getLine >>= readIO
