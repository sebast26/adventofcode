module Main where

import qualified Day01 (calibrationValue, realCalibrationValue)
import qualified Day02 (toGame, possibleGame, gameId, minimumSet, setProduct)

main :: IO ()
main = do
  contents <- readFile "inputs/01a"
  let calibrationLines = lines contents
      calibrationValues = map Day01.calibrationValue calibrationLines
      realCalibrationValues = map Day01.realCalibrationValue calibrationLines
  print $ sum calibrationValues
  print $ sum realCalibrationValues
  contents <- readFile "inputs/02a"
  let gameLines = lines contents
      games = map Day02.toGame gameLines
      possibleGames = filter Day02.possibleGame games
      gamesSum = foldl (\acc x -> acc + Day02.gameId x) 0 possibleGames
      minimumSets = map Day02.minimumSet games
      a = map Day02.setProduct minimumSets
  print gamesSum
  print $ sum a