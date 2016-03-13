{-# LANGUAGE Unsafe #-}

module GHCIWrap(GHCISession,
                runGHCI,
                startGHCI,
                stopGHCI,
                GHCI,
                ErrorMessage(..),
                SrcLoc(..),
                runStmt,
                runType,
                runTypeAt,
                runImport,
                runAddBreakpoint,
                runLoad,
                runReload,
                runDeleteStar,
                extractBreakpoints,
                runStmtWithTracing,
                TracingStep(..)) where

import System.Process
import System.Exit
import System.IO hiding (stdin, stdout, stderr)
import System.IO.Error
import System.Directory(getCurrentDirectory, getHomeDirectory)
import System.Environment(lookupEnv)
import System.FilePath((</>))
import Data.Char
import Text.Regex
import Text.Printf

-- import Control.Applicative
import Control.Monad
import Control.Exception hiding (handle)
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Class
import Data.Monoid

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Data.Set(Set)
import qualified Data.Set as S

import ErrorParser(parseErrors, ErrorMessage(..))
import HsParser(extractDecls)
import MiniHsParser(runMiniParser, parseTracingLine)

data GHCISession = GHCISession !Handle !Handle !Handle !ProcessHandle !FilePath

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
  fail e = GHCI $ \_ -> return $ Left [ErrorMessage "<unknown>" 1 1 e]

instance MonadIO GHCI where
  liftIO m = GHCI $ \_ -> m >>= return . Right

instance Applicative GHCI where
  pure = return
  (<*>) = ap

instance Functor GHCI where
  fmap = liftM

class GHCIResult a where
  readGHCI :: String -> a

getGHCICwd :: GHCI FilePath
getGHCICwd = GHCI $ \(GHCISession _ _ _ _ dir) -> return $ Right dir

sendGHCI :: String -> GHCI ()
sendGHCI what = GHCI $ \(GHCISession stdin _ _ _ _) -> do
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
        let promptRegex = mkRegex "^ ?(\\[[^]]+\\])?[ A-Za-z0-9.*]*>"
        line <- hReadUntil stdout "\n>"
        putStrLn $ "Read stdout: " ++ line
        case matchRegex promptRegex line of
          Nothing -> do
            rest <- readLoop
            return $ line ++ rest
          Just _ -> do
            _ <- hGetChar stdout  -- eat one more space
            return [] -- and eat the prompt
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

receiveGHCIRaw :: GHCI String
receiveGHCIRaw = GHCI $ \(GHCISession _ stdout stderr _ _) -> do
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

checkExc :: (e -> Bool) -> e -> Maybe e
checkExc f e = case f e of
  True -> Just e
  False -> Nothing

splitBy :: Eq a => [a] -> a -> [[a]]
splitBy [] _ = []
splitBy list v = case span (/= v) list of
  ~(first, rest) -> first : case rest of
    [] -> []
    (_:rs) -> splitBy rs v

startGHCI :: FilePath -> IO GHCISession
startGHCI targetDir = do
  cwd <- getCurrentDirectory
  home <- getHomeDirectory
  maybeHaskellPkgSandboxes <- lookupEnv "HASKELL_PACKAGE_SANDBOXES"
  let pkgdb = concatMap (\x -> ["-package-db", x]) $ case maybeHaskellPkgSandboxes of
        Just v -> filter (not . null) $ splitBy v ':'
        Nothing -> [cwd </> ".stack-work/install/x86_64-linux/lts-5.1/7.10.3/pkgdb",
                    home </> ".stack/snapshots/x86_64-linux/lts-5.1/7.10.3/pkgdb"]
      args = ["exec", "ghci-ng", "--", "-XSafe", "-XNoImplicitPrelude"] ++ pkgdb
      process = (proc' "stack" args) { std_in = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe,
                                       cwd = Just targetDir }
  -- print pkgdb
  (Just stdin, Just stdout, Just stderr, handle) <- createProcess process
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr LineBuffering
  let session = GHCISession stdin stdout stderr handle targetDir
  putStrLn "GHCI started"
  readErrors stderr
  x <- tryJust (checkExc isEOFError) $ readUntilPrompt stdout
  case x of
    Right _ -> return ()
    Left e -> do readErrors stderr
                 throwIO e
  putStrLn "Got GHCI prompt"
  runGHCI session $ do
    runGHCICommand_ ":set +c" -- required for ghci-ng
    let preludedir = cwd </> "prelude"
    runGHCICommand_ $ ":set \"-i" ++ preludedir ++ "\""
    runGHCICommand_ $ ":l \"" ++ (preludedir </> "Prelude.hs") ++ "\""
    runGHCICommand_ ":set -XImplicitPrelude"
    runGHCICommand_ ":set -main-is Prelude.hiddenDummyMain"
    runGHCICommand_ "import qualified System.TIO"
  return session

stopGHCI :: GHCISession -> IO ExitCode
stopGHCI (GHCISession stdin stdout stderr handle _) = do
  putStrLn "done"
  try $ hPutStrLn stdin ":q" :: IO (Either IOError ())
  try $ hClose stdin :: IO (Either IOError ())
  try $ hClose stdout :: IO (Either IOError ())
  try $ hClose stderr :: IO (Either IOError ())
  waitForProcess handle

-- emacs indenter gets confused by proc for some reason
proc' :: FilePath -> [String] -> CreateProcess
proc' = proc

newtype StmtResult = StmtResult { getStmtResult :: String }
instance GHCIResult StmtResult where
  readGHCI = StmtResult

data ExecStyle = PureFunction | TIO | Reject

prepareToRunStmt :: String -> GHCI ExecStyle
prepareToRunStmt stmt = do
  runGHCICommand_ "System.TIO.wipeSandbox"
  runGHCICommand_ $ "let it = (" ++ stmt ++ ")"
  type_ <- runType "it"
  return $ case extractMainType type_ of
    mainType | startsWith mainType "TIO" -> TIO
             | startsWith mainType "IO" -> Reject
             | startsWith mainType "GHC.Types.IO" -> Reject
             | otherwise -> PureFunction

runWithStyle :: ExecStyle -> GHCI String
runWithStyle PureFunction = runGHCICommand "print it" >>= return . getStmtResult
runWithStyle TIO = runGHCICommand "runTIO it" >>= return . getStmtResult
runWithStyle Reject = fail "Unsandboxed IO action is not allowed"

runStmt :: String -> GHCI String
runStmt stmt = prepareToRunStmt stmt >>= runWithStyle

parseType :: String -> String
parseType = go 0
  where
    go :: Int -> String -> String
    go lvl what = case lex what of
      [("", _)] -> undefined
      [("(", rest)] -> go (lvl+1) rest
      [(")", rest)] | lvl > 0 -> go (lvl-1) rest
                    | otherwise -> undefined
      [("::", rest)] | lvl == 0 -> dropWhile isSpace rest
      [(_, rest)] -> go lvl rest
      _ -> undefined

extractMainType :: String -> String
extractMainType from = case dropConstraint from of
  Just afterConstraint -> afterConstraint
  Nothing -> from
  where
    dropConstraint [] = Nothing
    dropConstraint ('=':'>':xs) = Just $ dropWhile isSpace xs
    dropConstraint (_:xs) = dropConstraint xs

newtype TypeResult = TypeResult { getTypeResult :: String }
instance GHCIResult TypeResult where
  readGHCI = TypeResult . parseType

runType :: String -> GHCI String
runType expr = runGHCICommand (":t " ++ expr) >>= return . getTypeResult

runAddBreakpoint :: String -> GHCI ()
runAddBreakpoint expr = runGHCICommand_ (":break " ++ expr)

runImport :: String -> GHCI ()
runImport imp = runGHCICommand_ ("import " ++ imp)

runLoad :: String -> GHCI String
runLoad file = runGHCICommand (":l \"" ++ file ++ "\"") >>= return . getStmtResult

runReload :: GHCI String
runReload = runGHCICommand (":r") >>= return . getStmtResult

runDeleteStar :: GHCI ()
runDeleteStar = runGHCICommand_ ":delete *"

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
data TracingResult = Stopped !TracingStep !String | Done !String deriving (Eq, Ord, Show, Read)

parseTracingStep :: String -> TracingResult
parseTracingStep step =
  let
    (output, stoppedAndVars) = break (`startsWith` "Stopped at ") $ lines step
  in
   case stoppedAndVars of
     [] -> Done (unlines output)
     (stopped:vars) -> Stopped (TS (parseStopped stopped) (M.fromList $ map parseOneResult vars)) (unlines output)
  where
    parseStopped :: String -> (FilePath, Int)
    parseStopped line =
      let after = drop (length "Stopped at ") line
          (filename, rest) = break (== ':') after
          (lineNo, _) = break (== ':') $ tail rest
      in case lineNo of
        '(':xs -> let (actualLineNo, _) = break (== ',') xs
                  in (filename, parse actualLineNo)
        x -> (filename, parse x)
    -- a version of read with sensible error messages
    parse :: Read a => String -> a
    parse what = case reads what of
      [(x, _)] -> x
      _ -> error $ "cannot parse " ++ what
    parseOneResult :: String -> (String, String)
    parseOneResult line =
      case runMiniParser parseTracingLine line of
        Just (name, value) -> (name, value)
        Nothing -> ("*failed to parse*", "")

data TracingCmd = Init | Step | ForceResult deriving (Eq, Ord, Show, Read, Enum)

runStmtWithTracing :: FilePath -> String -> GHCI ([TracingStep], String)
runStmtWithTracing filePath stmt = do
  runDeleteStar
  cwd <- getGHCICwd
  breakpoints <- ioToGHCI $ extractBreakpoints (cwd </> filePath)
  liftIO $ putStrLn $ "Breakpoints: " ++ show breakpoints
  forM (S.toList breakpoints) runAddBreakpoint
  let loop step out = do
        output <- lift $ nextTracingCommand stmt step
        case parseTracingStep output of
          Done res -> return (out <> res)
          Stopped map res -> do
            tell [map]
            loop (nextTracingState map) (out <> res)
  (res, map) <- runWriterT $ loop Init mempty
  return (map, res)
  where
    nextTracingCommand :: String -> TracingCmd -> GHCI String
    nextTracingCommand stmt Init = runStmt stmt
    nextTracingCommand _ Step = runGHCICommandRaw ":step"
    nextTracingCommand _ ForceResult = runGHCICommandRaw ":force _result"

    nextTracingState :: TracingStep -> TracingCmd
    nextTracingState (TS (path, _) _) =
      if path == filePath
      then Step
      else ForceResult

data SrcLoc = SrcLoc !Int !Int

runTypeAt :: FilePath -> SrcLoc -> SrcLoc -> String -> GHCI String
runTypeAt filePath (SrcLoc line_start col_start) (SrcLoc line_end col_end) stmt = do
  res <- runGHCICommand $ printf ":type-at %s %d %d %d %d %s" filePath line_start col_start line_end col_end stmt
  return $ getStmtResult res
