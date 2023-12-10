module Main where

import qualified Day01 (calibrationValue, realCalibrationValue)
import qualified Day02 (toGame, possibleGame, gameId, minimumSet, setProduct)
import qualified Day03 (parseSchematic, engineLineNumbers, gearNumbers)
import qualified Day04 (parseCard, won, scratchCardsWon, countCards)
import qualified Day05 (readSeeds, readMapEntry, almanac, seedToLocation, rangesOfSeeds, seedsFromRanges)
import Data.Ix (Ix(range))
import Data.List (foldl', foldl1')

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
  let   testSeeds = Day05.readSeeds "79 14 55 13"
        s2s = map Day05.readMapEntry $ lines "50 98 2\n52 50 48"
        s2f = map Day05.readMapEntry $ lines "0 15 37\n37 52 2\n39 0 15"
        f2w = map Day05.readMapEntry $ lines "49 53 8\n0 11 42\n42 0 7\n57 7 4"
        w2l = map Day05.readMapEntry $ lines "88 18 7\n18 25 70"
        l2t = map Day05.readMapEntry $ lines "45 77 23\n81 45 19\n68 64 13"
        t2h = map Day05.readMapEntry $ lines "0 69 1\n1 0 69"
        h2l = map Day05.readMapEntry $ lines "60 56 37\n56 93 4"
        alm = Day05.almanac testSeeds s2s s2f f2w w2l l2t t2h h2l
        locations = map (Day05.seedToLocation alm) testSeeds
  s2sContent <- readFile "inputs/05s2s"
  s2fContent <- readFile "inputs/05s2f"
  f2wContent <- readFile "inputs/05f2w"
  w2lContent <- readFile "inputs/05w2l"
  l2tContent <- readFile "inputs/05l2t"
  t2hContent <- readFile "inputs/05t2h"
  h2lContent <- readFile "inputs/05h2l"
  let   seeds = Day05.readSeeds "2637529854 223394899 3007537707 503983167 307349251 197383535 3543757609 276648400 2296792159 141010855 116452725 5160533 2246652813 49767336 762696372 160455077 3960442213 105867001 1197133308 38546766"
        s2s = map Day05.readMapEntry $ lines s2sContent
        s2f = map Day05.readMapEntry $ lines s2fContent 
        f2w = map Day05.readMapEntry $ lines f2wContent
        w2l = map Day05.readMapEntry $ lines w2lContent
        l2t = map Day05.readMapEntry $ lines l2tContent
        t2h = map Day05.readMapEntry $ lines t2hContent
        h2l = map Day05.readMapEntry $ lines h2lContent
        alm = Day05.almanac seeds s2s s2f f2w w2l l2t t2h h2l
        locations = map (Day05.seedToLocation alm) seeds 
        ranges = Day05.rangesOfSeeds seeds 
        newSeeds = concat $ Day05.seedsFromRanges ranges
        alm2 = Day05.almanac newSeeds s2s s2f f2w w2l l2t t2h h2l 
        -- newLocations = minimum $ map (Day05.seedToLocation alm2) newSeeds
        -- minLocation = foldl' (\acc x -> if x < acc then x else acc) 10000000000 $ map (Day05.seedToLocation alm2) newSeeds
        -- m = foldl1' min $ map (Day05.seedToLocation alm2) newSeeds
        m = foldl' (\acc x -> do
                let loc = Day05.seedToLocation alm2 x
                if loc < acc then loc else acc
            ) 1000000000 newSeeds
  print $ "Day05 part 1: " ++ show (minimum locations)
  print $ "Day05 part 2: " ++ show m
