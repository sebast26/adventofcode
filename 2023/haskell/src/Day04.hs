module Day04 (parseCard, won) where
import Parse ( parse )

data Card = Card { cardId :: Int
                 , winning :: [Int]
                 , numbers :: [Int]
                 , won :: [Int]
                 } deriving (Show)

parseCard :: [Char] -> Card
parseCard line = Card (read cId :: Int) winNum numNum wonNum
    where   (cId, w, n) = parse "Card {}: {} | {}" line :: (String, String, String) 
            winNum = map (\x -> read x :: Int) $ words w
            numNum = map (\x -> read x :: Int) $ words n
            wonNum = wonNumbers winNum numNum

wonNumbers :: [Int] -> [Int] -> [Int]
wonNumbers win nums = filter (/= -1) temp 
    where   temp = map (\w -> if w `elem` nums then w else -1) win