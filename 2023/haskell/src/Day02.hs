module Day02 (toGame, possibleGame, gameId, minimumSet, setProduct) where

import Parse (parse)
import Data.List.Utils (split)
import Data.String.Utils (strip)

data Cube = Red | Green | Blue | Unknown deriving (Show, Eq)
type Cubes = (Cube, Int)
type Set = [Cubes]
data Game = Game { gameId :: Int
                 , sets :: [Set]
                 } deriving (Show)

toCubes :: String -> Cubes
toCubes s
    | color == "red" = (Red, read amountS)
    | color == "green" = (Green, read amountS)
    | color == "blue" = (Blue, read amountS)
    | otherwise = (Unknown, 0)
    where (amountS, color) = parse "{} {}" s :: (String, String)

toSet :: String -> Set
toSet s = map (toCubes . strip) (split "," s)

toGame :: String -> Game
toGame s =
    Game gameId (map toSet setS)
    where (idS, game) = parse "Game {}: {}" s :: (String, String)
          gameId = read idS :: Int
          setS = map strip $ split ";" game

possibleCubes :: Cubes -> Bool
possibleCubes (Red, x) = x <= 12
possibleCubes (Green, x) = x <= 13
possibleCubes (Blue, x) = x <= 14
possibleCubes (Unknown, _) = False

possibleSet :: Set -> Bool
possibleSet = foldl (\acc x -> acc && possibleCubes x) True

possibleGame :: Game -> Bool
possibleGame g = foldl (\acc x -> acc && possibleSet x) True $ sets g 

minimumSet :: Game -> Set
minimumSet g = [(Red, maxR), (Green, maxG), (Blue, maxB)]
    where cubes = concat $ sets g
          maxR = foldl (\acc x -> if fst x == Red && snd x > acc then snd x else acc) 0 cubes
          maxG = foldl (\acc x -> if fst x == Green && snd x > acc then snd x else acc) 0 cubes
          maxB = foldl (\acc x -> if fst x == Blue && snd x > acc then snd x else acc) 0 cubes 
    
setProduct :: Set -> Int
setProduct = foldl (\acc x -> acc * snd x) 1
