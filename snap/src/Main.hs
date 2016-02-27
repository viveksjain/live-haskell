-- https://github.com/snapframework/snap/blob/0.14-stable/project_template/barebones/src/Main.hs
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Snap.Extras.JSON

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString as BS (ByteString, empty, writeFile)
import Data.List as List (elemIndices)
import Data.Map.Strict as Map (Map, fromList, mapKeys, map, null, toList)
import Data.Maybe (fromMaybe)
import Data.Text as Text (pack)
import Debug.Trace
import System.IO
import System.Process

main :: IO ()
main = startSession >>= quickHttpServe . site

site :: ReplSession -> Snap ()
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

evalHandler :: ReplSession -> Snap ()
evalHandler session = do
  param' <- getParam "script"
  case param' of
    Nothing -> return ()
    Just param -> do
      evalOutput <- liftIO $ run session param :: Snap EvalOutput
      writeJSON evalOutput

run :: ReplSession -> ByteString -> IO EvalOutput
run session script = do
  BS.writeFile "/tmp/test.hs" script
  reload session

startSession :: IO ReplSession
startSession = do
  BS.writeFile "/tmp/test.hs" BS.empty
  (Just hin, Just hout, Just herr, hproc) <-
    createProcess (proc "ghci" ["/tmp/test.hs"]) {
        std_in  = CreatePipe,
        std_out = CreatePipe,
        std_err = CreatePipe
    }
  hSetBuffering hin NoBuffering
  hSetBuffering hout NoBuffering
  hSetBuffering herr NoBuffering
  return $ ReplSession hin hout herr hproc

data ReplSession = ReplSession {
  in_   :: Handle,
  out  :: Handle,
  err  :: Handle,
  ph :: ProcessHandle
}

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

reload :: ReplSession -> IO EvalOutput
reload (ReplSession hin hout herr _) = do
    hPutStrLn hin ":r"
    hFlush hin
    out <- readLines hout
    err <- readLines herr
    return $ EvalOutput out err $ Map.fromList . (fromMaybe []) $ extractErrorDetail err

readLines :: Handle -> IO String
readLines h = loop
  where
    loop :: IO String
    loop = do
      done <- hReady h
      if done
        then do
          line <- hGetLine h
          rest <- loop
          return $ line ++ "\n" ++ rest
        else return []

extractErrorDetail :: String -> Maybe [(Int, String)]
extractErrorDetail s =
  let idxs = List.elemIndices ':' s :: [Int]
      slice :: Int -> Int -> String
      slice start end = take (end - (start+1)) . drop (start+1) $ s
      parse :: String -> Maybe Int
      parse input = case reads input :: [(Int, String)] of
        [(i, "")] -> Just i
        _ -> Nothing
      extractLineNumber :: Maybe Int
      extractLineNumber = parse $ slice (idxs!!0) (idxs!!1)
  in
    if length idxs < 3
    then Nothing
    else extractLineNumber >>= \lineNum -> Just [(lineNum, slice (1 + idxs!!2) (length s))]
