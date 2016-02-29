{-# LANGUAGE Trustworthy #-}

module SafePrelude where

import Prelude hiding (IO, readFile, writeFile, appendFile, putChar, putStr, putStrLn, print, getChar, getLine, getLine, getContents, interact, readIO, readLn)

import System.TIO(TIO, readFile, writeFile, appendFile)

type IO = TIO

-- FINISHME missing IO functions
