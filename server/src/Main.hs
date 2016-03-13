 -- https://github.com/snapframework/snap/blob/0.14-stable/project_template/barebones/src/Main.hs
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Snap.Extras.JSON

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types(Pair)
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as Vector (fromList)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe (fromMaybe)
import qualified Data.Text as Text (pack)
import Control.Exception
import System.IO (Handle, IOMode(WriteMode))
import qualified System.Directory as Dir (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), takeDirectory)
import qualified System.IO as IO (hClose, hFlush, openFile, openTempFile, readFile)
import qualified System.IO.Temp as IO.Temp (createTempDirectory)
import qualified SystemPathCopy as Path (copyDir)
import Control.Concurrent.MVar

import GHCIWrap

main :: IO ()
main = bracket (startGHCI "../test") stopGHCI $ \initialSession -> do
  mvar <- newMVar initialSession
  putStrLn "GHCI ready"
  runGHCI initialSession $ do
    runLoad "./app/Main.hs"
    runStmtWithTracing "app/Main.hs" "main" >>= liftIO . print
  putStrLn "runGHCI ok"
  -- runGHCI session $ runLoad "/tmp/test.hs" This should go in the new open handler
  quickHttpServe $ site mvar

site :: MVar GHCISession -> Snap ()
site session =
    route [ ("", serveDirectory "../client")
          , ("reload", method POST $ reloadHandler session)
          , ("type-at", method POST $ typeAtHandler session)
          , ("command", method POST $ commandHandler session)
          --, ("open", method POST $ openHandler)
          -- , ("openStack/:dirname/:filename", openStackHandler session) --for testing only
          , ("open", method POST $ openStackHandler session)
          , ("trace", method POST $ traceHandler session)
          , ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ]

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

createTempFileHandle :: IO Handle
createTempFileHandle = IO.openTempFile "/tmp/" "live-haskell.hs" >>= return . snd

openFileHandle :: FilePath -> IO (FilePath, Handle)
openFileHandle fp = IO.openFile fp WriteMode >>= \h -> return (fp,h)

reloadHandler :: MVar GHCISession -> Snap ()
reloadHandler mvar = do
  param'    <- getParam "script"
  filename' <- getParam "filename"
  (fp, h)   <- case filename' of
    Just filePath | not . BS.null $ filePath ->
          liftIO $ openFileHandle . BC.unpack $ filePath
    _  -> liftIO $ IO.openTempFile "/tmp/live-haskell/" "lh.hs"
  case param' of
    Nothing -> return ()
    Just param -> do
      evalOutput <- liftIO $ writeAndLoad mvar (fp, h) param :: Snap EvalOutput
      writeJSON evalOutput


commandHandler :: MVar GHCISession -> Snap ()
commandHandler mvar = do
  param' <- getParam "script"
  let param = BC.unpack . (Maybe.fromMaybe "main") $ param'
  stmtResult <- liftIO $ withMVar mvar $ \session -> (runGHCI session $ runStmt param)
  let evalOutput = decodeResult stmtResult
  writeJSON evalOutput

decodeResult :: Either [ErrorMessage] String -> EvalOutput
decodeResult res = case res of
  Left errs     -> EvalOutput "" "" $ Map.fromListWith (++) $ map (\(ErrorMessage _ line _ msg) -> (line, msg)) errs
  Right result  -> EvalOutput result "" Map.empty

decodeTraceResult :: Either [ErrorMessage] ([TracingStep], String) -> TraceOutput
decodeTraceResult (Left errs) = TraceOutput "" [] $ Map.fromListWith (++) $ map (\(ErrorMessage _ line _ msg) -> (line, msg)) errs
decodeTraceResult (Right (ts, s)) = TraceOutput s ts Map.empty

typeAtHandler :: MVar GHCISession -> Snap ()
typeAtHandler mvar = do
  filename'   <- getParam "filename"
  line_start' <- getParam "line_start"
  col_start'  <- getParam "col_start"
  line_end'   <- getParam "line_end"
  col_end'    <- getParam "col_end"
  text'       <- getParam "text"
  case (filename', line_start', col_start', line_end', col_end', text') of
    (Just filename, Just line_start, Just col_start, Just line_end, Just col_end, Just text) -> do
      let typeAt = runTypeAt (BC.unpack filename) start end (BC.unpack text)
          start = (SrcLoc (read $ BC.unpack line_start) (read $ BC.unpack col_start))
          end = (SrcLoc (read $ BC.unpack line_end) (read $ BC.unpack col_end))
      stmtResult <- liftIO $ withMVar mvar $ \session -> runGHCI session typeAt
      let evalOutput = decodeResult stmtResult
      writeJSON evalOutput
    _ -> return ()

openHandler :: Snap ()
openHandler =
  let param :: Maybe BS.ByteString -> BS.ByteString
      param p = Maybe.fromMaybe BS.empty p
      read :: BS.ByteString -> IO OpenOutput
      read fp | BS.null fp = return NoFilePathSupplied
              | otherwise = do
                let fp' = BC.unpack fp :: FilePath
                result <- try (IO.readFile fp') :: IO (Either SomeException String)
                case result of
                  Left ex -> return . OpenError $ ex
                  Right s -> return $ FileContents fp' fp' s
  in
    getParam "filename" >>= liftIO . read . param >>= writeJSON

openStackHandler :: MVar GHCISession -> Snap ()
openStackHandler mvar = do
  oldsession <- liftIO $ takeMVar mvar
  liftIO $ stopGHCI oldsession
  let param :: Maybe BS.ByteString -> BS.ByteString
      param p = Maybe.fromMaybe BS.empty p
      read :: BS.ByteString -> BS.ByteString -> IO OpenOutput
      read dp fp | BS.null dp = do
                    (d,f,s) <- createNewProjectDirectory
                    newsession <- startGHCI d
                    putMVar mvar newsession
                    runGHCI newsession $ runLoad "app/Main.hs"
                    return $ FileContents d f s
                 | BS.null fp = return NoFilePathSupplied
                 | otherwise = do
                    let dp' = BC.unpack dp :: FilePath
                        fp' = BC.unpack fp :: FilePath
                    isStack <- isStackProject dp' :: IO Bool
                    case isStack of
                      False -> return NotStackProject -- commented out while testing without UI changes
                      -- False -> do
                      --   (d,f,s) <- createNewProjectDirectory
                      --   runGHCI session (runCd d)
                      --   return $ FileContents d f s
                      True  -> do
                        result <- try (IO.readFile (dp' </> fp')) :: IO (Either SomeException String)
                        case result of
                          Left ex -> return . OpenError $ ex
                          Right s -> do
                            newsession <- startGHCI dp'
                            putMVar mvar newsession
                            runGHCI newsession $ runLoad (dp' </> fp')
                            return $ FileContents dp' fp' s
  filename' <- getParam $ "filename" :: Snap (Maybe BS.ByteString)
  filename  <- return . param $ filename'
  file      <- return . getFilename $ filename
  dirname   <- return . getDirpath $ filename
  liftIO (read dirname file) >>= writeJSON

traceHandler :: MVar GHCISession -> Snap ()
traceHandler mvar = do
  param'    <- getParam "script"
  filename' <- getParam "filename"
  let param    = BC.unpack . (Maybe.fromMaybe "main") $ param' :: String
      filename = BC.unpack . (Maybe.fromMaybe "Main.hs") $ filename' :: FilePath
      res   = withMVar mvar $ \session -> runGHCI session $ runStmtWithTracing filename param :: IO (Either [ErrorMessage] ([TracingStep], String))
  stmtResult <- liftIO $ res
  writeJSON . decodeTraceResult $ stmtResult

createNewProjectDirectory :: IO (FilePath, FilePath, String)
createNewProjectDirectory = do
  cwd <- Dir.getCurrentDirectory
  let from = cwd </> ".." </> "test"
  to <- IO.Temp.createTempDirectory "/tmp" "live-haskell" :: IO FilePath
  Path.copyDir from to
  let file = to </> "app" </> "Main.hs"
  readFile file >>= \s -> return (to, "app" </> "Main.hs", s)

isStackProject :: FilePath -> IO Bool
isStackProject "/" = return False
isStackProject fp = do
  exists <- Dir.doesFileExist $ fp </> "stack.yaml"
  if exists
    then return True
    else isStackProject $ takeDirectory fp

writeAndLoad :: MVar GHCISession -> (FilePath, Handle) -> ByteString -> IO EvalOutput
writeAndLoad mvar (filePath,h) script = withMVar mvar $ \session -> do
  BS.hPut h script
  IO.hFlush h
  IO.hClose h
  res <- runGHCI session $ do
    runLoad filePath -- runReload
  return . decodeResult $ res

data EvalOutput = EvalOutput {
  output :: String,         -- from std_out
  error :: String,          -- from std err
  errors :: Map Int String  -- map from line numbers to error messages
}


instance ToJSON EvalOutput where
  toJSON (EvalOutput out err errs) = object ["output" .= out, "error" .= err, "errors" .= errs]

instance (Show k, ToJSON v) => ToJSON (Map k v) where
  toJSON m | Map.null m = object []
           | otherwise  = object . Map.toList . Map.map toJSON . (Map.mapKeys (\k -> Text.pack . show $ k)) $ m

data OpenOutput = FileContents FilePath FilePath String | OpenError SomeException | NoFilePathSupplied | NotStackProject

data TraceOutput = TraceOutput {
  trace_output :: String,
  steps :: [TracingStep],
  trace_errors :: Map Int String  -- map from line numbers to error messages
}

instance ToJSON OpenOutput where
  toJSON (NoFilePathSupplied)  = object ["endpoint" .= s2j "/open", "status" .= s2j "error", "details" .= s2j "no file path supplied by user"]
  toJSON (NotStackProject)  = object ["endpoint" .= s2j "/open", "status" .= s2j "error", "details" .= s2j "not a stack project"]
  toJSON (OpenError errorMessage)  = object ["endpoint" .= s2j "/open", "status" .= s2j "error", "details" .= show errorMessage]
  toJSON (FileContents dirpath file fileContents) = object ["endpoint" .= s2j "/open", "status" .= s2j "success", "details" .= fileContents, "dir" .= dirpath, "file" .= file]

stringToJSON :: String -> Value
stringToJSON s = String . Text.pack $ s

-- a much shorter alias for stringToJSON
s2j :: String -> Value
s2j = stringToJSON

getFilename :: ByteString -> ByteString
getFilename f = last $ BC.split '/' f

getDirpath :: ByteString -> ByteString
getDirpath f = BC.intercalate "/" . init $ BC.split '/' f

instance ToJSON TraceOutput where
  toJSON (TraceOutput out steps errs) = object ["output" .= out, "steps" .= steps, "errors" .= errs]

instance ToJSON TracingStep where
  toJSON (TS (_, lineNo) vars)  = Array $ Vector.fromList [toJSON lineNo, object (prettyVars vars)]
    where
      prettyVars :: Map String String -> [Pair]
      prettyVars m | Map.null m = []
                   | otherwise  = Map.foldrWithKey (\k a b -> (Text.pack k .= Text.pack a) : b ) [] m
