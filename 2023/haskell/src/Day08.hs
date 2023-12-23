module Day08 where
import Parse (parse)
import qualified Data.Map as Map

parseInstructions :: [Char] -> [(b, b) -> b]
parseInstructions [] = []
parseInstructions ('L':xs) = fst : parseInstructions xs
parseInstructions ('R':xs) = snd : parseInstructions xs
parseInstructions (_:xs) = parseInstructions xs

parseMap :: String -> Map.Map String (String, String)
parseMap s = foldl (\acc (k, v1, v2) -> Map.insert k (v1, v2) acc) Map.empty  entries
    where   entries = map (\x -> parse "{} = ({}, {})" x :: (String, String, String)) $ lines s

routeZZZ :: (Foldable t, Num a1) => Map.Map [Char] a2 -> t (a2 -> [Char]) -> (a1, [Char])
routeZZZ m = foldl (\(step, currentNode) command -> do
                        let tup = m Map.! currentNode
                            nextNode = command tup
                        if nextNode == "ZZZ" then (step, nextNode) else (step + 1, nextNode)
                    ) (1, "AAA")

routeZZZ' :: Map.Map [Char] a -> [a -> [Char]] -> Int -> [Char] -> Int
routeZZZ' m instList step currentNode
    | nextNode == "ZZZ" = step
    | otherwise = routeZZZ' m (tail instList) (step+1) nextNode
    where   tup = m Map.! currentNode
            command = head instList
            nextNode = command tup