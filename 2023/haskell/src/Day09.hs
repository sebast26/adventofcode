module Day09 where
import Data.List (nub)

findNextInSequence :: [Int] -> Int
findNextInSequence values = findNext values []

findFirstInSequence :: [Int] -> Int
findFirstInSequence values = findFirst values []

findNext :: [Int] -> [[Int]] -> Int
findNext values tables
    | allZero = extrapolate tables
    | otherwise = findNext table (values:tables)
    where   allZero = tables /= [] && nub (head tables) == [0]
            table = diffTable values


diffTable :: Num a => [a] -> [a]
diffTable table = map (\i -> table !! (i + 1) - table !! i) [0..(length table - 2)]

extrapolate :: (Foldable t, Num a) => t [a] -> a
extrapolate = foldl (\acc t -> acc + last t) 0

findFirst :: [Int] -> [[Int]] -> Int
findFirst values tables
    | allZero = extrapolateFirst tables
    | otherwise = findFirst table (values:tables)
    where   allZero = tables /= [] && nub (head tables) == [0]
            table = diffTable values

extrapolateFirst :: (Foldable t, Num a) => t [a] -> a
extrapolateFirst = foldl (\acc t -> head t - acc) 0