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
import qualified Data.Text as Text (pack)
import Control.Exception

import GHCIWrap

main :: IO ()
main = bracket startGHCI stopGHCI $ \session -> do
  putStrLn "GHCI ready"
  runGHCI session $ do
    runLoad "test.hs"
    runStmtWithTracing "test.hs" "main" >>= liftIO . print
  putStrLn "runGHCI ok"
  --quickHttpServe $ site session

site :: GHCISession -> Snap ()
site session =
    route [ ("", serveDirectory "/home/neeral/Documents/cs240h/project/live-haskell/client")
          , ("evaluate", method POST $ evalHandler session)
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

run :: GHCISession -> ByteString -> IO EvalOutput
run session script = do
  res <- runGHCI session $ do
    runStmt (BC.unpack script)
  case res of
    Left errs -> return $ EvalOutput "" "" $ Map.fromListWith (++) $ map (\(ErrorMessage _ line _ msg) -> (line, msg)) errs
    Right result -> return $ EvalOutput result "" Map.empty

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
