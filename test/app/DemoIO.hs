{-# LANGUAGE OverloadedStrings #-}

import Data.Text

main = do
  input <- getTest
  putStrLn input
  let output = unpack $ replace "hello" "bye" $ pack input
  putStrLn output

getTest = readFile "/Users/vivek/Desktop/test.txt"

writeTest = writeFile "/Users/vivek/Desktop/test.txt"

printTest = getTest >>= putStrLn
