module Main where

import qualified MyLib (someFunc)
import qualified Day01 (add)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let sum = Day01.add 1 2
  MyLib.someFunc
  print sum
