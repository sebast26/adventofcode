module Main where

import qualified Day01 (calibrationValue)

main :: IO ()
main = do
  contents <- readFile "inputs/01a"
  let calibrationLines = lines contents
      calibrationValues = map (Day01.calibrationValue) calibrationLines
  print $ sum calibrationValues