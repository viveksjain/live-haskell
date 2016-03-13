{-# LANGUAGE Safe #-}

module SafePrelude(module Prelude, TIO, runTIO, IO, readFile, writeFile, appendFile, hiddenDummyMain, putChar, putStr, putStrLn, print, getChar, getLine, getContents, interact, readIO, readLn) where

import qualified System.IO as SIO (IO)
import Prelude hiding (IO, readFile, writeFile, appendFile, putChar, putStr, putStrLn, print, getChar, getLine, getContents, interact, readIO, readLn)

import System.TIO(TIO, runTIO, readFile, writeFile, appendFile, putChar, putStr, putStrLn, print, getChar, getLine, getContents, interact, readIO, readLn)

type IO = TIO

hiddenDummyMain :: SIO.IO ()
hiddenDummyMain = return ()
