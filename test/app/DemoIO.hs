{-# LANGUAGE OverloadedStrings #-}

import Data.Text

main = do
  input <- getTest
  putStrLn input
  let output = unpack $ replace "hello" "bye" $ pack input
  putStrLn output

getTest = readFile "./test.txt"

writeTest = writeFile "./test.txt"

printTest = getTest >>= putStrLn
