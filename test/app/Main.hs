{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Main where

import System.IO
import Data.Array.IO
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Monoid

import Lib

bar = return=<<([1::Int,2,3]++[4,5])

invalid :: String
invalid = "7"

add :: Int -> Int -> Int
add !x y = x + y 

add' :: Num a => a -> a -> a
add' = (+)

add'' :: Int -> Int -> Int
add'' x' = add x'

foo = "bla" <> "bla" 

main :: IO ()
main = do
  putStrLn $ BC.unpack foo
  putStrLn $ invalid
  writeFile "/tmp/foo" "bla"
  print $ add' 2.3 4.7
  print $ add undefined 3 * add' 1 3 * add'' 0 1