module Day09 where
import Data.List (nub)

findNextInSequence values = findNext values []

findNext values tables
    | allZero = tables
    | otherwise = findNext table (values:tables)
    where   allZero = tables /= [] && nub (head tables) == [0]
            table = diffTable values


diffTable :: Num a => [a] -> [a]
diffTable table = map (\i -> table !! (i + 1) - table !! i) [0..(length table - 2)]