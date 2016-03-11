-- https://github.com/snapframework/snap/blob/0.14-stable/project_template/barebones/src/Main.hs
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Snap.Extras.JSON

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe (fromMaybe)
import qualified Data.Text as Text (pack)
import Control.Exception
import System.IO (Handle, IOMode(WriteMode))
import qualified System.IO as IO (hClose, hFlush, openFile, openTempFile, readFile)

import GHCIWrap

main :: IO ()
main = bracket (startGHCI "../test") stopGHCI $ \session -> do
  putStrLn "GHCI ready"
  runGHCI session $ do
    runLoad "./app/Main.hs"
    runStmtWithTracing "app/Main.hs" "main'" >>= liftIO . print
  putStrLn "runGHCI ok"
  -- runGHCI session $ runLoad "/tmp/test.hs" This should go in the new open handler
  quickHttpServe $ site session

site :: GHCISession -> Snap ()
site session =
    route [ ("", serveDirectory "../client")
          , ("evaluate", method POST $ evalHandler session)
          , ("type-at", method POST $ typeAtHandler session)
          , ("command", method POST $ commandHandler session)
          , ("open", method POST $ openHandler)
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

evalHandler :: GHCISession -> Snap ()
evalHandler session = do
  param'    <- getParam "script"
  filename' <- getParam "filename"
  (fp, h)   <- case filename' of
    Just filePath | not . BS.null $ filePath ->
          liftIO $ openFileHandle . BC.unpack $ filePath
    _  -> liftIO $ IO.openTempFile "/tmp/" "live-haskell.hs"
  case param' of
    Nothing -> return ()
    Just param -> do
      evalOutput <- liftIO $ run session (fp, h) param :: Snap EvalOutput
      writeJSON evalOutput


commandHandler :: GHCISession -> Snap ()
commandHandler session = do
  param' <- getParam "script"
  let param = BC.unpack . (Maybe.fromMaybe "main") $ param'
  stmtResult <- liftIO $ runGHCI session $ runStmt param
  let evalOutput = decodeResult stmtResult
  writeJSON evalOutput

decodeResult :: Either [ErrorMessage] String -> EvalOutput
decodeResult res = case res of
  Left errs     -> EvalOutput "" "" $ Map.fromListWith (++) $ map (\(ErrorMessage _ line _ msg) -> (line, msg)) errs
  Right result  -> EvalOutput result "" Map.empty

typeAtHandler :: GHCISession -> Snap ()
typeAtHandler session = do
  line_start' <- getParam "line_start"
  col_start'  <- getParam "col_start"
  line_end'   <- getParam "line_end"
  col_end'    <- getParam "col_end"
  text'       <- getParam "text"
  case (line_start', col_start', line_end', col_end', text') of
    (Just line_start, Just col_start, Just line_end, Just col_end, Just text) -> do
      let typeAt = runTypeAt "/tmp/test.hs" start end (BC.unpack text)
          start = (SrcLoc (read $ BC.unpack line_start) (read $ BC.unpack col_start))
          end = (SrcLoc (read $ BC.unpack line_end) (read $ BC.unpack col_end))
      stmtResult <- liftIO $ runGHCI session typeAt
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
                result <- try (IO.readFile . BC.unpack $ fp) :: IO (Either SomeException String)
                case result of
                  Left ex -> return . OpenError $ ex
                  Right s -> return . FileContents $ s
  in
    getParam "filename" >>= liftIO . read . param >>= writeJSON

run :: GHCISession -> (FilePath, Handle) -> ByteString -> IO EvalOutput
run session (filePath,h) script = do
  BS.hPut h script
  IO.hFlush h
  IO.hClose h
  res <- runGHCI session $ do
    runLoad2 filePath -- runReload
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

data OpenOutput = FileContents String | OpenError SomeException | NoFilePathSupplied

instance ToJSON OpenOutput where
  toJSON (NoFilePathSupplied)  = object ["endpoint" .= s2j "/open", "status" .= s2j "passthrough", "details" .= s2j "no file path supplied by user"]
  toJSON (OpenError errorMessage)  = object ["endpoint" .= s2j "/open", "status" .= s2j "error", "details" .= show errorMessage]
  toJSON (FileContents fileContents) = object ["endpoint" .= s2j "/open", "status" .= s2j "success", "details" .= fileContents]

stringToJSON :: String -> Value
stringToJSON s = String . Text.pack $ s

-- a much shorter alias for stringToJSON
s2j :: String -> Value
s2j = stringToJSON
