module Main where

import Data.Ix (Ix (range))
import Data.List (foldl', foldl1', intercalate, sort)
import qualified Day01 (calibrationValue, realCalibrationValue)
import qualified Day02 (gameId, minimumSet, possibleGame, setProduct, toGame)
import qualified Day03 (engineLineNumbers, gearNumbers, parseSchematic)
import qualified Day04 (countCards, parseCard, scratchCardsWon, won)
import qualified Day05 (almanac, rangesOfSeeds, readMapEntry, readSeeds, seedToLocation, seedsFromRanges)
import qualified Day06 (race, simulations, wonSims)
import Day07 (HandBid (bid), parseJLine)
import qualified Day07 (parseLine)
import qualified Day08 (parseInstructions, parseMap, routeXXZ, routeXXZ'', routeZZZ, routeZZZ', startingNodes)
import GHC.Read (readField)
import qualified Day09

main :: IO ()
main = do
    contents01 <- readFile "inputs/01a"
    let     calibrationLines = lines contents01
            calibrationValues = map Day01.calibrationValue calibrationLines
            realCalibrationValues = map Day01.realCalibrationValue calibrationLines
    print $ sum calibrationValues
    print $ sum realCalibrationValues

    contents02 <- readFile "inputs/02a"
    let     gameLines = lines contents02
            games = map Day02.toGame gameLines
            possibleGames = filter Day02.possibleGame games
            gamesSum = foldl (\acc x -> acc + Day02.gameId x) 0 possibleGames
            minimumSets = map Day02.minimumSet games
            a = map Day02.setProduct minimumSets
    print gamesSum
    print $ sum a

    contents03 <- readFile "inputs/03a"
    let     schematic = Day03.parseSchematic $ lines contents03
            engineNumbers = Day03.engineLineNumbers schematic
            s = sum $ map sum engineNumbers
            gearNumbers = Day03.gearNumbers schematic
            gearSum = sum $ map (uncurry (*)) gearNumbers
    print s
    print gearSum

    content04 <- readFile "inputs/04a"
    let     cards = map Day04.parseCard $ lines content04
            cardsValue = filter (not . null) $ map Day04.won cards
            s = sum $ map (\won -> 2 ^ (length won - 1)) cardsValue
            wonCards = map Day04.scratchCardsWon cards
            countSum = sum $ Day04.countCards wonCards
    print $ "Day04 part 1: " ++ show s
    print $ "Day04 part 2: " ++ show countSum

    let     testSeeds = Day05.readSeeds "79 14 55 13"
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
    let     seeds = Day05.readSeeds "2637529854 223394899 3007537707 503983167 307349251 197383535 3543757609 276648400 2296792159 141010855 116452725 5160533 2246652813 49767336 762696372 160455077 3960442213 105867001 1197133308 38546766"
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
            -- we skip this for later `cabal run`s, since it takes some time to compute these
            -- m = foldl' (\acc x -> do
            --        let loc = Day05.seedToLocation alm2 x
            --        if loc < acc then loc else acc
            --    ) 1000000000 newSeeds
    print $ "Day05 part 1: " ++ show (minimum locations)
    -- print $ "Day05 part 2: " ++ show m

    let     r1 = Day06.race (40, 219)
            r2 = Day06.race (81, 1012)
            r3 = Day06.race (77, 1365)
            r4 = Day06.race (72, 1089)
            races = [r1, r2, r3, r4]
            sims = map Day06.simulations races
            won = zipWith Day06.wonSims races sims
            result = foldl (\acc x -> length x * acc) 1 won
            bigRace = Day06.race (40817772, 219101213651089)
            sim = Day06.simulations $ bigRace
            result2 = length $ Day06.wonSims bigRace sim
    print $ "Day06 part 1: " ++ show result
    print $ "Day06 part 2: " ++ show result2

    content07test <- readFile "inputs/07test"
    content07 <- readFile "inputs/07a"
    let     handBids = map Day07.parseLine $ lines content07
            sortedHB = sort handBids
            res = sum $ zipWith (\hb idx -> idx * bid hb) sortedHB [1 ..]
            handBidsP2 = map Day07.parseJLine $ lines content07
            sortedHBP2 = sort handBidsP2
            resP2 = sum $ zipWith (\hb idx -> idx * bid hb) sortedHBP2 [1 ..]
    print $ "Day07 part 1: " ++ show res
    print $ "Day07 part 2: " ++ show resP2

    content08 <- readFile "inputs/08a"
    content08btest <- readFile "inputs/08btest"
    let     instructions = "LRLRRLRLRRRLRRRLRRLRLLRLRLRRRLRLRRLLRRLLRRRLLRRRLRRRLRRLLRLRRRLRRLRLRLLRRLLRRRLLRLRRRLRRRLLRLRRRLLRLLRRLRLRRRLLRLRLLRRRLLRLRRRLLLRRRLLLRRLLLRRRLLRLRLRLRRLLRRRLRRLRRRLRRLRRRLRLRRLRLRRRLRLRRRLRRLRRRLRLLLRLRRRLRLLRLRRLRRRLRRLRLRLRLRRLRRLLRLLLRLRLRRRLRRRLLRLLRLRRLRRRLRRLRRRLRLRRRR"
            i = Day08.parseInstructions $ take 15000000 (drop 235000000 (cycle instructions))
            icycle = Day08.parseInstructions $ cycle instructions
            m = Day08.parseMap content08
            bTestInstr = "LR"
            ib = Day08.parseInstructions $ cycle bTestInstr
            mb = Day08.parseMap content08btest
            startingNodesB = Day08.startingNodes mb
            startingNodes = Day08.startingNodes m
            nextNodes1 = ["NKN", "CDF", "VTS", "LVQ", "CBB", "GRN"]
    print $ "Day08 starting nodes: " ++ show startingNodes
    print $ show (Day08.routeXXZ'' mb ib 0 "11A")
    print $ show (Day08.routeXXZ'' mb ib 0 "22A")
    print $ show (Day08.routeXXZ'' m icycle 0 "AAA")
    print $ show (Day08.routeXXZ'' m icycle 0 "BXA")
    print $ show (Day08.routeXXZ'' m icycle 0 "HCA")
    print $ show (Day08.routeXXZ'' m icycle 0 "LDA")
    print $ show (Day08.routeXXZ'' m icycle 0 "QTA")
    print $ show (Day08.routeXXZ'' m icycle 0 "SJA")
    print $ show
                ( foldl
                lcm
                1
                [   Day08.routeXXZ'' m icycle 0 "AAA",
                    Day08.routeXXZ'' m icycle 0 "BXA",
                    Day08.routeXXZ'' m icycle 0 "HCA",
                    Day08.routeXXZ'' m icycle 0 "LDA",
                    Day08.routeXXZ'' m icycle 0 "QTA",
                    Day08.routeXXZ'' m icycle 0 "SJA"
                ]
                )

    let inputTest1 = map (\x -> read x::Int) $ words "0 3 6 9 12 15"
        inputTest2 = map (\x -> read x::Int) $ words "1 3 6 10 15 21"
        inputTest3 = map (\x -> read x::Int) $ words "10 13 16 21 30 45"
        real1 = map (\x -> read x::Int) $ words "14 28 56 121 263 554 1131 2270 4549 9201 18855 39034 81081 167714 343319 692592 1373532 2674454 5110126 9579947 17622015"
    print $ "Day09 test1: " ++ show (Day09.findNextInSequence inputTest1)
    print $ "Day09 test2: " ++ show (Day09.findNextInSequence inputTest2)
    print $ "Day09 test3: " ++ show (Day09.findNextInSequence inputTest3)
    print $ "Day09 real1: " ++ show (Day09.findNextInSequence real1)
    content09 <- readFile "inputs/09.txt"
    let l = lines content09
        lNums = map (map (\y -> read y::Int) . words) l
        nums = map Day09.findNextInSequence lNums
    print $ "Day09 part 1: " ++ show (sum nums)