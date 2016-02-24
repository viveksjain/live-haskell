-- https://github.com/snapframework/snap/blob/0.14-stable/project_template/barebones/src/Main.hs
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Snap.Extras.JSON
import Debug.Trace
import System.Process
import System.IO
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Text as Text (pack)
import Data.ByteString as BS (ByteString, writeFile, hGetContents, null)
import Data.ByteString.Char8 as BC (unpack)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    route [ ("", serveDirectory "/home/neeral/Documents/cs240h/project/live-haskell/client")
          , ("evaluate", method POST evalHandler)
          , ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ]

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

evalHandler :: Snap ()
evalHandler = do
  param' <- getParam "script"
  case param' of
    Nothing -> return ()
    Just param -> do
      (typ, output) <- liftIO $ run param
      writeJSON $ object $ [(Text.pack typ) .= output]

run :: ByteString -> IO (String, String)
run script = do
  BS.writeFile "/tmp/test.hs" script
  eval

eval :: IO (String, String)
eval = do
  (_, Just hout, Just herr, _) <-
    createProcess (proc "runhaskell" ["/tmp/test.hs"]) {
        std_out = CreatePipe,
        std_err = CreatePipe
    }
  hSetBuffering hout NoBuffering
  hSetBuffering herr NoBuffering
  out <- BS.hGetContents hout
  err <- BS.hGetContents herr
  return $ deplex (out, err)
  where
    deplex :: (BS.ByteString, BS.ByteString) -> (String, String)
    deplex (out, err) = if BS.null out
      then ("error", BC.unpack err)
      else ("output", BC.unpack out)
