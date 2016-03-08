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
import qualified Data.List as List (intersperse)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text (pack)
import Control.Exception

import GHCIWrap

main :: IO ()
main = bracket startGHCI stopGHCI $ \session -> do
  putStrLn "GHCI ready"
  runGHCI session $ do
    runLoad "test.hs"
    runStmtWithTracing "test.hs" "main'" >>= liftIO . print
  putStrLn "runGHCI ok"
  runGHCI session $ runLoad "/tmp/test.hs"
  quickHttpServe $ site session

site :: GHCISession -> Snap ()
site session =
    route [ ("", serveDirectory "../client")
          , ("evaluate", method POST $ evalHandler session)
          , ("type-at", method POST $ typeAtHandler session)
          -- , ("type-at/:line_start/:col_start/:line_end/:col_end/:text",  typeAtHandler session) -- for testing without the UI
          , ("command", method POST $ commandHandler session)
          , ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ]

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

evalHandler :: GHCISession -> Snap ()
evalHandler session = do
  param' <- getParam "script"
  case param' of
    Nothing -> return ()
    Just param -> do
      evalOutput <- liftIO $ run session param :: Snap EvalOutput
      writeJSON evalOutput

commandHandler :: GHCISession -> Snap ()
commandHandler session = do
  stmtResult <- liftIO $ runGHCI session $ runStmt "main"
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



run :: GHCISession -> ByteString -> IO EvalOutput
run session script = do
  BS.writeFile "/tmp/test.hs" script
  res <- runGHCI session $ do
    runReload
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
