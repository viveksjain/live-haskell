{-# LANGUAGE Unsafe #-}

module GHCIWrap(GHCISession,
                withGHCI,
                runGHCI,
                startGHCI,
                stopGHCI,
                GHCI,
                ErrorMessage(..),
                runStmt,
                runType,
                runImport,
                runAddBreakpoint) where

import System.Process
import System.Exit
import System.IO
import Data.Char
import Text.Regex

import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class

data ErrorMessage = ErrorMessage FilePath Int Int String deriving (Eq, Ord, Read, Show)

newtype ErrorParser a = ErrorParser { runErrorParser :: String -> Maybe (a, String) }

instance Monad ErrorParser where
  return x = ErrorParser $ \s -> Just (x, s)
  m >>= f = ErrorParser $ \s ->
    case runErrorParser m s of
      Nothing -> Nothing
      Just (v, s') -> runErrorParser (f v) s'
  fail _ = ErrorParser $ const Nothing

instance Applicative ErrorParser where
  pure = return
  (<*>) = ap

instance Functor ErrorParser where
  fmap = liftM

expectChar :: Char -> ErrorParser ()
expectChar c = ErrorParser $ \s -> case s of
  (x:xs) | x == c -> Just ((), xs)
  _ -> Nothing

eatWhitespace :: ErrorParser ()
eatWhitespace = ErrorParser $ \s -> case s of
  [] -> Just ((), [])
  (x:xs) | isSpace x -> runErrorParser eatWhitespace xs
         | otherwise -> Just ((), xs)

eatUntil :: Char -> ErrorParser String
eatUntil c = ErrorParser $ \s -> case s of
  [] -> Just ([], [])
  (x:xs) | x == c -> Just ([], xs)
         | otherwise -> case runErrorParser (eatUntil c) xs of
    Nothing -> Nothing
    Just (v, xs) -> Just (x : v, xs)

peekNextChar :: ErrorParser (Maybe Char)
peekNextChar = ErrorParser $ \s -> case s of
  [] -> Just (Nothing, [])
  (x:xs) -> Just (Just x, x:xs)

parseOneError :: ErrorParser ErrorMessage
parseOneError = do
  filename <- eatUntil ':'
  line <- fmap read (eatUntil ':')
  col <- fmap read (eatUntil ':')
  msg1 <- eatUntil '\n'
  let readLoop = do
        mc <- peekNextChar
        case mc of
          Nothing -> return []
          Just c | isSpace c -> do
            msg <- eatUntil '\n'
            rest <- readLoop
            return $ msg ++ '\n' : rest
          _ -> return []
  msgrest <- readLoop
  let fullmsg = msg1 ++ '\n' : msgrest
  return $ ErrorMessage filename line col fullmsg

parseAllErrors :: ErrorParser [ErrorMessage]
parseAllErrors = do
  eatWhitespace
  mc <- peekNextChar
  case mc of
    Nothing -> return []
    Just _ -> do
      err <- parseOneError
      rest <- parseAllErrors
      return $ err : rest

parseErrors :: String -> [ErrorMessage]
parseErrors s = case runErrorParser parseAllErrors s of
  Nothing -> []
  Just (errors, _) -> errors

data GHCISession = GHCISession !Handle !Handle !Handle !ProcessHandle

newtype GHCI a = GHCI { getGHCI :: GHCISession -> IO (Either [ErrorMessage] a) }

runGHCI :: GHCISession -> GHCI a -> IO (Either [ErrorMessage] a)
runGHCI = flip getGHCI

instance Monad GHCI where
  return x = GHCI $ \_ -> return $ Right x
  m >>= f = GHCI $ \session -> do
    mv <- getGHCI m session
    case mv of
      Right v -> getGHCI (f v) session
      -- pay attention that these two Left have different types
      -- one is Either [ErrorMessage] a, the other is Either [ErrorMessage] b
      Left errs -> return $ Left errs

instance MonadIO GHCI where
  liftIO m = GHCI $ \_ -> m >>= return . Right

instance Applicative GHCI where
  pure = return
  (<*>) = ap

instance Functor GHCI where
  fmap = liftM

class GHCIResult a where
  readGHCI :: String -> a

sendGHCI :: String -> GHCI ()
sendGHCI what = GHCI $ \(GHCISession stdin _ _ _) -> do
  hPutStrLn stdin what
  return (Right ())

hReadUntil :: Handle -> String -> IO String
hReadUntil handle set = do
  let loop = do
        c <- hGetChar handle
        if elem c set
          then return [c]
          else do
          next <- loop
          return $ c : next
  loop

readUntilPrompt :: Handle -> IO String
readUntilPrompt stdout = do
  hWaitForInput stdout (-1)
  let readLoop :: IO String
      readLoop = do
        let promptRegex = mkRegex "^[ A-Za-z0-9*]+>"
        line <- hReadUntil stdout "\n>"
        putStrLn $ "Read stdout: " ++ line
        case matchRegex promptRegex line of
          Nothing -> do
            rest <- readLoop
            return $ line ++ rest
          Just _ -> return [] -- and eat the prompt
  readLoop

readErrors :: Handle -> IO String
readErrors stderr = do
  let readLoop :: IO String
      readLoop = do
        ready <- hReady stderr
        if ready
          then do
            line <- hGetLine stderr
            putStrLn $ "Read stderr: " ++ line
            rest <- readLoop
            return $ line ++ '\n' : rest
          else return []
  readLoop

receiveGHCI :: GHCIResult a => GHCI a
receiveGHCI = GHCI $ \(GHCISession _ stdout stderr _) -> do
  result <- readUntilPrompt stdout
  if null result
    then readErrors stderr >>= return . Left . parseErrors
    else return $ Right $ readGHCI result

newtype EmptyResult = EmptyResult { getEmptyResult :: () }
instance GHCIResult EmptyResult where
  readGHCI = const (EmptyResult ())

runGHCICommand :: GHCIResult a => String -> GHCI a
runGHCICommand cmd = do
  sendGHCI cmd
  receiveGHCI

runGHCICommand_ :: String -> GHCI ()
runGHCICommand_ cmd = do
  sendGHCI cmd
  receiveGHCI >>= return . getEmptyResult

startGHCI :: IO GHCISession
startGHCI = do
  let args = (proc' "/usr/bin/ghci" ["ghci"]) { std_in = CreatePipe,
                                                std_out = CreatePipe,
                                                std_err = CreatePipe }
  (Just stdin, Just stdout, Just stderr, handle) <- createProcess args
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr LineBuffering
  let session = GHCISession stdin stdout stderr handle
  readUntilPrompt stdout
  runGHCI session $ do
    runImport "Prelude ()"
  return session

stopGHCI :: GHCISession -> IO ExitCode
stopGHCI (GHCISession stdin stdout stderr handle) = do
  putStrLn "done"
  try $ hPutStrLn stdin ":q" :: IO (Either IOError ())
  try $ hClose stdin :: IO (Either IOError ())
  try $ hClose stdout :: IO (Either IOError ())
  try $ hClose stderr :: IO (Either IOError ())
  waitForProcess handle

withGHCI :: GHCI a -> IO (Either [ErrorMessage] a)
withGHCI action = do
  bracket startGHCI stopGHCI (getGHCI action)

-- emacs indenter gets confused by proc for some reason
proc' :: FilePath -> [String] -> CreateProcess
proc' = proc

newtype StmtResult = StmtResult { getStmtResult :: String }
instance GHCIResult StmtResult where
  readGHCI = StmtResult

runStmt :: String -> GHCI String
runStmt stmt = runGHCICommand stmt >>= return . getStmtResult

newtype TypeResult = TypeResult { getTypeResult :: String }
instance GHCIResult TypeResult where
  readGHCI = TypeResult . parseType
    where
      -- this parser is kind of dumb and would get confused by something like
      -- "::" :: [Char]
      parseType (':':':':tp) = dropWhile (isSpace) tp
      parseType [] = error "Missing type"
      parseType (x:xs) = parseType xs

runType :: String -> GHCI String
runType expr = runGHCICommand (":t " ++ expr) >>= return . getTypeResult

runAddBreakpoint :: String -> GHCI ()
runAddBreakpoint expr = runGHCICommand (":break " ++ expr) >>= return . getEmptyResult

runImport :: String -> GHCI ()
runImport imp = runGHCICommand ("import " ++ imp) >>= return . getEmptyResult
