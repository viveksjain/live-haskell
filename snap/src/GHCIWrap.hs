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
                runAddBreakpoint) where

import System.Process
import System.IO
import System.Posix.IO
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

newtype GHCI a = GHCI { getGHCI :: GHCISession -> IO a }

runGHCI :: GHCISession -> GHCI a -> IO a
runGHCI = flip getGHCI

instance Monad GHCI where
  return x = GHCI $ \_ -> return x
  m >>= f = GHCI $ \session -> do
    v <- getGHCI m session
    getGHCI (f v) session

instance MonadIO GHCI where
  liftIO m = GHCI $ \_ -> m

instance Applicative GHCI where
  pure = return
  (<*>) = ap

instance Functor GHCI where
  fmap = liftM

class GHCIResult a where
  readGHCI :: String -> a

sendGHCI :: String -> GHCI ()
sendGHCI what = GHCI $ \(GHCISession stdin _ _ _) -> hPutStrLn stdin what

hReadUntil :: Handle -> String -> IO String
hReadUntil handle set = do
  let loop = do
        c <- hGetChar handle
        if elem c set
          then return [c]
          else do
          next <- loop
          return c : next
  loop

readUntilPrompt :: GHCI String
readUntilPrompt = GHCI $ \(GHCISession _ stdout _ _) -> do
  hWaitForInput stdout (-1)
  let readLoop :: IO String
      readLoop = do
        let promptRegex = mkRegex "^[ A-Za-z0-9*]+>"
        line <- hReadUntil "\n>" stdout
        putStrLn $ "Read stdout: " ++ line
        case matchRegex promptRegex line of
          Nothing -> do
            rest <- readLoop
            return $ line ++ '\n' : rest
          Just _ -> return [] -- and eat the prompt
  readLoop

readErrors :: GHCI String
readErrors = GHCI $ \(GHCISession _ _ stderr _) -> do
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

receiveGHCI :: GHCIResult a => GHCI (Either [ErrorMessage] a)
receiveGHCI = do
  result <- readUntilPrompt
  if null result
    then readErrors >>= return . Left . parseErrors
    else return $ Right $ readGHCI result

runGHCICommand :: GHCIResult a => String -> GHCI (Either [ErrorMessage] a)
runGHCICommand cmd = do
  sendGHCI cmd
  receiveGHCI

startGHCI :: IO GHCISession
startGHCI = do
  let args = (proc' "/usr/bin/ghci" ["ghci", "-XSafe"]) { std_in = CreatePipe,
                                                          std_out = CreatePipe,
                                                          std_err = CreatePipe }
  (Just stdin, Just stdout, Just stderr, handle) <- createProcess args
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr LineBuffering
  let session = GHCISession stdin stdout stderr handle
  runGHCI session readUntilPrompt
  return session

stopGHCI :: GHCISession -> IO ()
stopGHCI (GHCISession stdin stdout stderr handle) = do
  putStrLn "done"
  try $ hPutStrLn stdin ":q" :: IO (Either IOError ())
  try $ hClose stdin :: IO (Either IOError ())
  try $ hClose stdout :: IO (Either IOError ())
  try $ hClose stderr :: IO (Either IOError ())
  waitForProcess handle

withGHCI :: GHCI a -> IO a
withGHCI action = do
  bracket startGHCI stopGHCI (getGHCI action)

-- emacs indenter gets confused by proc for some reason
proc' = proc

newtype StmtResult = StmtResult { getStmtResult :: String }

instance GHCIResult StmtResult where
  readGHCI = StmtResult

runStmt :: String -> GHCI (Either [ErrorMessage] String)
runStmt stmt = runGHCICommand stmt >>= return . fmap getStmtResult

newtype TypeResult = TypeResult { getTypeResult :: String }
instance GHCIResult TypeResult where
  readGHCI = TypeResult . parseType
    where
      -- this parser is kind of dumb and would get confused by something like
      -- "::" :: [Char]
      parseType ':':':':tp = dropWhile (isSpace) tp
      parseType [] = error "Missing type"
      parseType x:xs = parseType xs

runType :: String -> GHCI (Either [ErrorMessage] String)
runType expr = runGHCICommand (":t " ++ expr) >>= return . fmap getTypeResult

newtype EmptyResult = EmptyResult { getEmpty :: () }
instance GHCIResult EmptyResult where
  readGHCI = const (EmptyResult ())

runAddBreakpoint :: String -> GHCI (Either [ErrorMessage] ())
runAddBreakpoint expr = runGHCICommand (":break " ++ expr) >>= return . fmap getEmptyResult
