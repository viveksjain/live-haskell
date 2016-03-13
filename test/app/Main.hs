module Main where

import System.IO
import Data.Array.IO

import Lib

add :: Num a => a -> a -> a
add x y = x + y

add' :: Num a => a -> a -> a
add' = (+)

add'' :: Num a => a -> a -> a
add'' x = add x

main = do
  putStrLn "hello"
  writeFile "/tmp/foo" "bla"
  print $ add 2 3 * add' 1 3 * add'' 0 1

