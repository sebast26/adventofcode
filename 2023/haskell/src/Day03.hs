module Day03 (parseSchematic, engineLineNumbers) where

import Data.Char (isPunctuation, isSymbol, isDigit)
import Data.Sequence (mapWithIndex, fromList)
import Data.Foldable ( Foldable(toList) )
import qualified Data.Map as Map
import Data.List (intersect, nub)
import Data.Maybe (fromJust)

data Schematic = Schematic { digits :: Map.Map Int [Int]
                           , symbols :: Map.Map Int [Int]
                           , engineLineNumbers :: [[Int]]
                           } deriving (Show)

parseSchematic :: [String] -> Schematic
parseSchematic xs = Schematic digitMap symbolMap lineNumbers
    where symbolFn = \ch -> (isPunctuation ch || isSymbol ch) && ch /= '.'
          symbolMap = mapPredicate symbolFn xs
          digitMap = mapPredicate isDigit xs
          pEngineDigits = possibleEngineNumbers symbolMap
          eDigits = Map.intersectionWith intersect digitMap pEngineDigits
          inp = Map.fromList $ toList $ mapWithIndex (\i line -> (i + 1, line)) $ fromList xs
          lineDigitss = lineDigits inp eDigits
          lineNumbers = map (\(line, digits) -> numbers line digits) lineDigitss

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