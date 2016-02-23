-- https://github.com/snapframework/snap/blob/0.14-stable/project_template/barebones/src/Main.hs
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Snap.Extras.JSON
import Debug.Trace
import System.Process
import System.IO
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text
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
      output <- liftIO $ run $ BC.unpack param
      writeJSON $ object $ [("output" :: Text) .= output]

run :: String -> IO String
run script = do
  writeFile "/tmp/test.hs" script
  eval

eval :: IO String
eval = do
  (_, Just hout, _, _) <-
    createProcess (proc "runhaskell" ["/tmp/test.hs"]) {
        std_out = CreatePipe
    }
  hSetBuffering hout NoBuffering
  hGetContents hout
