module Main where

import qualified Day01 (calibrationValue, realCalibrationValue)

main :: IO ()
main = do
  contents <- readFile "inputs/01a"
  let calibrationLines = lines contents
      calibrationValues = map (Day01.calibrationValue) calibrationLines
      realCalibrationValues = map (Day01.realCalibrationValue) calibrationLines
  print $ sum calibrationValues
  print $ sum realCalibrationValues