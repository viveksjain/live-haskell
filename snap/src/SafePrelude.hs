{-# LANGUAGE Safe #-}

module SafePrelude(module Prelude, TIO, runTIO, IO, readFile, writeFile, appendFile) where

import Prelude hiding (IO, readFile, writeFile, appendFile, putChar, putStr, putStrLn, print, getChar, getLine, getLine, getContents, interact, readIO, readLn)

import System.TIO(TIO, runTIO, readFile, writeFile, appendFile)

type IO = TIO

-- FINISHME missing IO functions
