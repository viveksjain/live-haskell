{-# LANGUAGE BangPatterns #-}

import Data.Text

main = do
  putStrLn $ show $ factorial 5 1

factorial 0 acc = acc
factorial n !acc = factorial (n - 1) (acc * n)
 