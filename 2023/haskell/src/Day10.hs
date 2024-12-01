module Day10 where
import Data.List (elemIndex)
import Data.Maybe (fromJust)

type Pipe = Char
type PipeMap = [[Pipe]]
type Position = (Int, Int)

-- Utility methods

findStart :: PipeMap -> Position
findStart m = (sridx,scidx)
    where   (sridx, srow) = head $ filter (\(_,row) -> 'S' `elem` row) $ zip [0..(length m - 1)] m
            scidx = fromJust $ elemIndex 'S' srow

findAdjecent pos = []

pipeAt :: PipeMap -> Position -> Maybe Pipe
pipeAt m pos
    | rIdx < 0 || rIdx > length m - 1 = Nothing
    | cIdx < 0 || cIdx > length firstRow - 1 = Nothing
    | otherwise = Just $ row !! cIdx
    where   rIdx = fst pos
            cIdx = snd pos
            firstRow = head m
            row = m !! rIdx

