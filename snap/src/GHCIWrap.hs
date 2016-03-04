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
                runAddBreakpoint,
                runLoad,
                runDeleteStar,
                extractBreakpoints,
                runStmtWithTracing,
                TracingStep(..)) where

import System.Process
import System.Exit
import System.IO
import Data.Char
import Data.Maybe
import Text.Regex

import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Class

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Data.Set(Set)
import qualified Data.Set as S

import ErrorParser(parseErrors, ErrorMessage(..))
import HsParser(extractDecls)
import MiniHsParser(runMiniParser, parseTracingLine)

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
  putStrLn $ "Write stdin: " ++ what
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
        -- this regex is incorrect
        -- because the char class at the beginning should be [^\\]]
        -- not [^]]
        let promptRegex = mkRegex "^ ?(\\[[^]]+\\])?[ A-Za-z0-9*]+>"
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

flushErrors :: GHCI ()
flushErrors = GHCI $ \(GHCISession _ _ stderr _) -> do
  readErrors stderr
  return $ Right ()

receiveGHCIRaw :: GHCI String
receiveGHCIRaw = GHCI $ \(GHCISession _ stdout stderr _) -> do
  result <- readUntilPrompt stdout
  errors <- readErrors stderr
  if not $ null errors
    then return $ Left $ parseErrors errors
    else return $ Right $ result

receiveGHCI :: GHCIResult a => GHCI a
receiveGHCI = fmap readGHCI receiveGHCIRaw

newtype EmptyResult = EmptyResult { getEmptyResult :: () }
instance GHCIResult EmptyResult where
  readGHCI = const (EmptyResult ())

runGHCICommand :: GHCIResult a => String -> GHCI a
runGHCICommand cmd = do
  sendGHCI cmd
  receiveGHCI

runGHCICommandRaw :: String -> GHCI String
runGHCICommandRaw cmd = do
  sendGHCI cmd
  receiveGHCIRaw

runGHCICommand_ :: String -> GHCI ()
runGHCICommand_ cmd = do
  sendGHCI cmd
  receiveGHCI >>= return . getEmptyResult

startGHCI :: IO GHCISession
startGHCI = do
  let args = (proc' "stack" ["exec", "ghci"]) { std_in = CreatePipe,
                                                         std_out = CreatePipe,
                                                         std_err = CreatePipe }
  (Just stdin, Just stdout, Just stderr, handle) <- createProcess args
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr LineBuffering
  let session = GHCISession stdin stdout stderr handle
  putStrLn "GHCI started"
  readErrors stderr
  readUntilPrompt stdout
  putStrLn "Got GHCI prompt"
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

runLoad :: String -> GHCI ()
runLoad file = runGHCICommand (":l " ++ file) >>= return . getEmptyResult

runDeleteStar :: GHCI ()
runDeleteStar = runGHCICommand ":delete *" >>= return . getEmptyResult

extractBreakpoints :: FilePath -> IO (Either [ErrorMessage] (Set String))
extractBreakpoints path = do
  withFile path ReadMode $ \handle -> do
    contents <- hGetContents handle
    return $! extractDecls contents path

ioToGHCI :: IO (Either [ErrorMessage] a) -> GHCI a
ioToGHCI = GHCI . const

startsWith :: String -> String -> Bool
startsWith _ [] = True
startsWith [] _ = False
startsWith (x:xs) (y:ys) | x == y = startsWith xs ys
startsWith _ _ = False

data TracingStep = TS { getPosition :: !(FilePath, Int), getVars :: !(Map String String) } deriving (Eq, Ord, Show, Read)
data TracingResult = Stopped !TracingStep | Done !String deriving (Eq, Ord, Show, Read)

parseTracingStep :: String -> TracingResult
                                             -- note the space here: it's because we read until "Bla>",
                                             -- but the prompt is "Bla> "
parseTracingStep step | not (startsWith step " Stopped at ") = Done step
parseTracingStep step = Stopped $
  let
    stopped = head $ lines step
    vars = tail $ lines step
  in TS (parseStopped stopped) (M.fromList $ map parseOneResult vars)
  where
    parseStopped :: String -> (FilePath, Int)
    parseStopped line =
      let after = drop (length " Stopped at ") line
          (filename, rest) = break (== ':') after
          (lineNo, _) = break (== ':') $ tail rest
      in (filename, read lineNo)
    parseOneResult :: String -> (String, String)
    parseOneResult line =
      case runMiniParser parseTracingLine line of
        Just (name, value) -> (name, value)
        Nothing -> ("*failed to parse*", "")

type ListOutput a b = WriterT [a] GHCI b

data TracingState = Init | AtBreakpoint | AfterStep | AfterForce deriving (Eq, Ord, Show, Read, Enum)

runStmtWithTracing :: FilePath -> String -> GHCI ([TracingStep], String)
runStmtWithTracing filePath stmt = do
  runDeleteStar
  breakpoints <- ioToGHCI $ extractBreakpoints filePath
  liftIO $ putStrLn $ "Breakpoints: " ++ show breakpoints
  forM (S.toList breakpoints) runAddBreakpoint
  let loop step = do
        (nextStep, output) <- lift $ nextTracingCommand stmt step
        case parseTracingStep output of
          Done res -> return res
          Stopped map -> do
            tell [map]
            loop nextStep
  (res, map) <- runWriterT $ loop Init
  return (map, res)
  where
    nextTracingCommand :: String -> TracingState -> GHCI (TracingState, String)
    nextTracingCommand stmt Init = do
      res <- runGHCICommandRaw stmt
      return (AtBreakpoint, res)
    nextTracingCommand _ AtBreakpoint = do
      res <- runGHCICommandRaw ":step"
      return (AfterStep, res)
    --nextTracingCommand _ AfterStep = do
    --  res <- runGHCICommandRaw ":force _result"
    --  return (AfterForce, res)
    nextTracingCommand _ AfterStep = do
      res <- runGHCICommandRaw ":cont"
      return (AtBreakpoint, res)
