module Day04 (parseCard, won, scratchCardsWon, countCards) where
import Parse ( parse )
import Data.Sequence (mapWithIndex, fromList)
import Data.Foldable (Foldable(toList))

data Card = Card { cardId :: Int
                 , winning :: [Int]
                 , numbers :: [Int]
                 , won :: [Int]
                 , scratchCardsWon :: Int
                 } deriving (Show)

parseCard :: [Char] -> Card
parseCard line = Card (read cId :: Int) winNum numNum wonNum scWon
    where   (cId, w, n) = parse "Card {}: {} | {}" line :: (String, String, String)
            winNum = map (\x -> read x :: Int) $ words w
            numNum = map (\x -> read x :: Int) $ words n
            wonNum = wonNumbers winNum numNum
            scWon = length wonNum

wonNumbers :: [Int] -> [Int] -> [Int]
wonNumbers win nums = filter (/= -1) temp
    where   temp = map (\w -> if w `elem` nums then w else -1) win

-- [0,10,10,9,0,8,7,3,1,9,0,2,0,0,4,1,1,3,0,0,0,10,1,3,0,7,6,8,7,3,3,4,1,0,1,0,9,1,10,3,5,2,6,2,1,0,4,2,2,1,0,10,7,1,10,10,10,5,0,5,8,2,5,10,1,2,5,5,6,5,0,0,1,0,0,10,10,1,10,3,10,0,10,4,10,3,1,7,6,10,1,8,4,0,3,3,3,0,0,0,6,10,3,10,1,0,10,7,8,1,3,2,2,2,2,1,0,6,10,1,7,4,10,2,5,3,8,0,7,0,2,3,1,2,0,0,10,4,7,4,8,9,2,10,10,0,7,0,3,5,7,6,6,5,4,3,0,0,0,7,10,10,10,10,10,5,10,3,4,10,8,4,10,9,9,4,1,3,3,0,1,2,1,0,10,8,10,10,4,1,5,10,8,1,4,9,3,4,6,4,0,2,3,1,0,0]

countCards :: Num b => [Int] -> [b]
countCards wonCount = count
    where   init = map (const 1) wonCount
            wonCountIndex = toList $ mapWithIndex (,) $ fromList wonCount
            count = foldl (\acc (i, won) -> do
                            toList $ mapWithIndex (\accI accV -> do
                                let num = acc !! i 
                                if accI > i && accI <= i + won 
                                    then accV + num
                                    else accV
                                ) $ fromList acc
                            ) init wonCountIndex
