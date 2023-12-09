module Main where

import qualified Day01 (calibrationValue, realCalibrationValue)
import qualified Day02 (toGame, possibleGame, gameId, minimumSet, setProduct)
import qualified Day03 (parseSchematic, engineLineNumbers, gearNumbers)
import qualified Day04 (parseCard, won, scratchCardsWon, countCards)

main :: IO ()
main = do
  contents01 <- readFile "inputs/01a"
  let calibrationLines = lines contents01
      calibrationValues = map Day01.calibrationValue calibrationLines
      realCalibrationValues = map Day01.realCalibrationValue calibrationLines
  print $ sum calibrationValues
  print $ sum realCalibrationValues
  contents02 <- readFile "inputs/02a"
  let gameLines = lines contents02
      games = map Day02.toGame gameLines
      possibleGames = filter Day02.possibleGame games
      gamesSum = foldl (\acc x -> acc + Day02.gameId x) 0 possibleGames
      minimumSets = map Day02.minimumSet games
      a = map Day02.setProduct minimumSets
  print gamesSum
  print $ sum a
  contents03 <- readFile "inputs/03a"
  let schematic = Day03.parseSchematic $ lines contents03
      engineNumbers = Day03.engineLineNumbers schematic
      s = sum $ map sum engineNumbers
      gearNumbers = Day03.gearNumbers schematic
      gearSum = sum $ map (uncurry (*)) gearNumbers
  print s
  print gearSum
  content04 <- readFile "inputs/04a"
  let cards = map Day04.parseCard $ lines content04
      cardsValue = filter (not . null) $ map Day04.won cards
      s = sum $ map (\won -> 2 ^ (length won - 1)) cardsValue
      wonCards = map Day04.scratchCardsWon cards
      countSum = sum $ Day04.countCards wonCards
  print $ "Day04 part 1: " ++ show s
  print $ "Day04 part 2: " ++ show countSum