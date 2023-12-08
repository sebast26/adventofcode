module Day03 (parseSchematic, engineLineNumbers, gearNumbers) where

import Data.Char (isPunctuation, isSymbol, isDigit)
import Data.Sequence (mapWithIndex, fromList)
import Data.Foldable ( Foldable(toList) )
import qualified Data.Map as Map
import Data.List (intersect)
import Data.Maybe (fromJust)

data Schematic = Schematic { digits :: Map.Map Int [Int]
                           , symbols :: Map.Map Int [Int]
                           , input :: Map.Map Int String
                           , engineDigits :: Map.Map Int [Int]
                           , engineLineNumbers :: [[Int]]
                           , gearNumbers :: [(Int, Int)]
                           } deriving (Show)

parseSchematic :: [String] -> Schematic
parseSchematic xs = Schematic digitMap symbolMap inp eDigits lineNumbers gearNumbers
    where symbolFn = \ch -> (isPunctuation ch || isSymbol ch) && ch /= '.'
          symbolMap = mapPredicate symbolFn xs
          digitMap = mapPredicate isDigit xs
          pEngineDigits = possibleEngineNumbers symbolMap
          eDigits = Map.intersectionWith intersect digitMap pEngineDigits
          inp = Map.fromList $ toList $ mapWithIndex (\i line -> (i + 1, line)) $ fromList xs
          lineDigitss = lineDigits inp eDigits
          lineNumbers = map (\(line, digits) -> numbers line digits) lineDigitss
          gearMap = mapPredicate (== '*') xs
          possibleGearDigits = possibleEngineNumbers gearMap
          gearManyDigitsWithDups = Map.intersectionWith intersect digitMap possibleGearDigits
          gearManyDigits = Map.map removeConsecutiveNumbers gearManyDigitsWithDups
          gearDigits = gearAdjacentTwo gearMap gearManyDigits
          gearNumbers = gearDigitsToNumbers gearDigits inp

gearDigitsToNumbers gearDigits inp = 
    map (\twoDigits -> do
            let firstDigitCoord = head twoDigits
            let secondDigitCoord = last twoDigits
            let firstLine = fromJust $ Map.lookup (fst firstDigitCoord) inp
            let secondLine = fromJust $ Map.lookup (fst secondDigitCoord) inp
            let firstNumber = number firstLine (snd firstDigitCoord)
            let secondNumber = number secondLine (snd secondDigitCoord)
            (firstNumber, secondNumber)
        ) gearDigits

mapPredicate :: (Char -> Bool) -> [String] -> Map.Map Int [Int]
mapPredicate fn xs = Map.fromList (toList s)
    where s = mapWithIndex (\i x -> (i + 1, findInLine fn x)) $ fromList xs

findInLine :: (Char -> Bool) -> String -> [Int]
findInLine fn = snd . foldl (\(c, l) ch
        -> if fn ch then (c + 1, l ++ [c]) else (c + 1, l) ) (1, [])

symbolArea :: (Int, Int) -> [(Int, [Int])]
symbolArea (r, c) = [(r-1, columns), (r, columns), (r+1, columns)]
    where columns = [c-1, c, c + 1]

-- given line number (Int) and symbol positions ([Int])
-- it returns possible engine digits
possibleInLine :: Int -> [Int] -> [(Int, [Int])]
possibleInLine k vs = Map.toList $ Map.unionsWith (++) m
    where m = map (\v -> Map.fromList $ symbolArea (k, v)) vs

possibleEngineNumbers :: Map.Map Int [Int] -> Map.Map Int [Int]
possibleEngineNumbers symb = Map.unionsWith (++) mm
    where allPossible = Map.mapWithKey possibleInLine symb
          mm = map Map.fromList (Map.elems allPossible)

lineDigits :: Map.Map Int String -> Map.Map Int [Int] -> [(String, [Int])]
lineDigits inp enDigits = cmb
    where sz = Map.size inp
          cmb = map (\idx -> do
                    let inpLine = fromJust $ Map.lookup idx inp
                    let eDLine = fromJust $ Map.lookup idx enDigits
                    let eDLineWithoutDups = removeConsecutiveNumbers eDLine
                    (inpLine, eDLineWithoutDups)
                    ) [1..sz]

number :: [Char] -> Int -> Int
number line initPosition = read allDigits
    where listPosition = initPosition - 1
          numDigits = [line !! listPosition]
          toLeft = digitsLeft line listPosition numDigits
          allDigits = digitsRight line listPosition toLeft

digitsLeft :: [Char] -> Int -> [Char] -> [Char]
digitsLeft line initPosition acc
    | initPosition - 1 < 0 = acc
    | isDigit leftChar = digitsLeft line (initPosition - 1) (leftChar : acc)
    | otherwise = acc
    where leftChar = line !! (initPosition - 1)

digitsRight :: [Char] -> Int -> [Char] -> [Char]
digitsRight line initPosition acc
    | initPosition + 1 >= length line = acc
    | isDigit rightChar = digitsRight line (initPosition + 1) (acc ++ [rightChar])
    | otherwise = acc
    where rightChar = line !! (initPosition + 1)

numbers :: [Char] -> [Int] -> [Int]
numbers line = map (number line)

removeConsecutiveNumbers :: [Int] -> [Int]
removeConsecutiveNumbers inp = filter (/= -1) noDups
    where diffs = 0 : differences inp
          noDups = map (\idx -> do
                    let dv = diffs !! idx
                    if dv /= 1 then inp !! idx else -1
            ) [0..length inp - 1]

differences :: Num a => [a] -> [a]
differences [] = []
differences [_] = []
differences (x:y:xs) = (y - x) : differences (y:xs)

gearAdjacentTwo :: Map.Map Int [Int] -> Map.Map Int [Int] -> [[(Int, Int)]]
gearAdjacentTwo starMap digitsMap = mm
    where   cumbMap = Map.mapWithKey (\ln starsPos -> do
                    let starClusters = map (\pos -> adjacentDigits (ln, pos) digitsMap) starsPos
                    filter (\xs -> length xs == 2) starClusters
                  ) starMap
            m1 = map snd $ Map.toList cumbMap
            mm = concat m1

adjacentDigits :: (Int, Int) -> Map.Map Int [Int] -> [(Int, Int)]
adjacentDigits (ln, pos) digitsMap = filter (/= (-1, -1)) [cUL, cUC, cUR, cCL, cCC, cCR, cDL, cDC, cDR]
    where   upDigits = Map.findWithDefault [] (ln-1) digitsMap
            ceDigits = Map.findWithDefault [] ln digitsMap
            downDigits = Map.findWithDefault [] (ln+1) digitsMap
            cUL = if (pos - 1) `elem` upDigits then (ln-1, pos-1) else (-1, -1)
            cUC = if pos `elem` upDigits then (ln-1, pos) else (-1, -1)
            cUR = if (pos + 1) `elem` upDigits then (ln-1, pos+1) else (-1, -1)
            cCL = if (pos - 1) `elem` ceDigits then (ln, pos-1) else (-1, -1)
            cCC = if pos `elem` ceDigits then (ln, pos) else (-1, -1)
            cCR = if (pos + 1) `elem` ceDigits then (ln, pos+1) else (-1, -1)
            cDL = if (pos - 1) `elem` downDigits then (ln+1, pos-1) else (-1, -1)
            cDC = if pos `elem` downDigits then (ln+1, pos) else (-1, -1)
            cDR = if (pos + 1) `elem` downDigits then (ln+1, pos+1) else (-1, -1)