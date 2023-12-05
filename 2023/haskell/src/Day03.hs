module Day03 (parseSchematic) where

import Data.Char (isPunctuation, isSymbol)
import Data.Sequence (mapWithIndex, fromList)
import Data.Foldable ( Foldable(toList) )
import qualified Data.Map as Map 

data Schematic = Schematic { numbers :: Int
                           , symbols :: Map.Map Int [Int]
                           } deriving (Show)

parseSchematic :: [String] -> Schematic
parseSchematic xs = Schematic 1 sMap
    where sMap = symbolMap xs

symbolMap :: [String] -> Map.Map Int [Int] 
symbolMap xs = Map.fromList (toList s) 
    where s = mapWithIndex (\i x -> (i + 1, lineSymbols x)) $ fromList xs

lineSymbols :: String -> [Int]
lineSymbols = snd . foldl (\(c, l) ch
        -> if (isPunctuation ch || isSymbol ch) && ch /= '.' then
            (c + 1, l ++ [c])
           else
            (c + 1, l) )
           (1, [])