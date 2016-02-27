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
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Text as Text (pack)
import Data.ByteString as BS (ByteString, writeFile, hGetContents, null, empty)
import Data.ByteString.Char8 as BC (pack, unpack)
import Data.List as List (elemIndices)

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
      (typ, output) <- liftIO $ run session param
      writeJSON $ object $ [(Text.pack typ) .= output]

run :: ReplSession -> ByteString -> IO (String, String)
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

reload :: ReplSession -> IO (String, String)
reload (ReplSession hin hout herr _) = do
    hPutStrLn hin ":r\n"
    hFlush hin
    out <- readLines hout
    err <- readLines herr
    return $ deplex (out, err)
  where
    deplex :: (String, String) -> (String, String)
    deplex (out, err) = if err == []
      then ("output", out)
      else ("error", err)

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

extractLineNumber :: String -> Maybe Int
extractLineNumber s =
  let idxs = List.elemIndices ':' s :: [Int]
      slice :: Int -> Int -> String
      slice start end = take (end - (start+1)) . drop (start+1) $ s
      parse :: String -> Maybe Int
      parse input =
        case reads input :: [(Int, String)] of
          [(i, "")] -> Just i
          _ -> Nothing
  in
    if length idxs < 2
    then Nothing
    else parse $ slice (idxs!!0) (idxs!!1)
