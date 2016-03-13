{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Main where

import System.IO
import Data.Array.IO
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Monoid

import Lib

add :: Num a => a -> a -> a
add x y = x + y

add' :: Num a => a -> a -> a
add' = (+)

add'' :: Num a => a -> a -> a
add'' x = add x

foo = "bla" <> BC.pack "bla"

main :: IO ()
main = do
  putStrLn $ BC.unpack foo
  writeFile "/tmp/foo" "bla"
  print $ add 2 3 * add' 1 3 * add'' 0 1
