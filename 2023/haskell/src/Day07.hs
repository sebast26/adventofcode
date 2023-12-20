{-# LANGUAGE InstanceSigs #-}
module Day07 where

import Data.List (nub, sort, group)
import Data.Maybe (fromJust)
import Data.List.Utils (replace)

data Strenth = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord)

data Type = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show, Eq, Ord)

data Hand = Hand    { handType :: Type
                    , cards :: [Strenth]
                    } deriving (Show)

instance Eq Hand where
    (==) :: Hand -> Hand -> Bool
    (Hand t1 c1) == (Hand t2 c2) =
        if t1 == t2 then c1 == c2 else t1 == t2

instance Ord Hand where
    compare :: Hand -> Hand -> Ordering
    (Hand t1 c1) `compare` (Hand t2 c2) =
        if t1 == t2 then c1 `compare` c2 else t1 `compare` t2

isFiveOfAKind :: [Strenth] -> Bool
isFiveOfAKind s = length (nub s) == 1

isJFiveOfAKind :: [Strenth] -> Bool
isJFiveOfAKind s = isFiveOfAKind s || isFiveOfAKind cardsNoJoker
    where   nonJoker = head $ filter (/= Joker) s
            cardsNoJoker = replace [Joker] [nonJoker] s

isFourOfAKind :: Ord a => [a] -> Bool
isFourOfAKind s = length groups == 2 && 1 `elem` sizes && 4 `elem` sizes
    where   groups = group $ sort s
            sizes = map length groups

isJFourOfAKind :: [Strenth] -> Bool
isJFourOfAKind s = isFourOfAKind s || any isFourOfAKind possible4
    where   possible4 = map (\x -> replace [Joker] [x] s) $ nub $ filter (/= Joker) s


isFullHouse :: Ord a => [a] -> Bool
isFullHouse s = length groups == 2 && 2 `elem` sizes && 3 `elem` sizes
    where   groups = group $ sort s
            sizes = map length groups

isJFullHouse :: [Strenth] -> Bool
isJFullHouse s = isFullHouse s || any isFullHouse possibleFH
    -- TODO: czy kilka Jokerów to kilka tych samych kart czy mogą być rózne?
    where   possibleFH = map (\x -> replace [Joker] [x] s) $ nub $ filter (/= Joker) s

isThreeOfAKind :: Ord a => [a] -> Bool
isThreeOfAKind s = length groups >= 2 && 3 `elem` sizes
    where   groups = group $ sort s
            sizes = map length groups

isJThreeOfAKind :: [Strenth] -> Bool
isJThreeOfAKind s = isThreeOfAKind s || any isThreeOfAKind possible3
    where   possible3 = map (\x -> replace [Joker] [x] s) $ nub $ filter (/= Joker) s

isTwoPair :: Ord a => [a] -> Bool
isTwoPair s = length groups == 3 && sortSizes == [1,2,2]
    where   groups = group $ sort s
            sizes = map length groups
            sortSizes = sort sizes

isJTwoPair :: [Strenth] -> Bool
isJTwoPair s = isTwoPair s || any isTwoPair possible2
    where   possible2 = map (\x -> replace [Joker] [x] s) $ nub $ filter (/= Joker) s

isOnePair :: Ord a => [a] -> Bool
isOnePair s = length groups == 4 && sortSized == [1, 1, 1, 2]
    where   groups = group $ sort s
            sizes = map length groups
            sortSized = sort sizes

isJOnePair :: [Strenth] -> Bool
isJOnePair s = isOnePair s || any isOnePair possible1
    where   possible1 = map (\x -> replace [Joker] [x] s) $ nub $ filter (/= Joker) s

isHighCard :: Ord a => [a] -> Bool
isHighCard s = length groups == 5
    where   groups = group $ sort s

data HandBid = HandBid  { hand :: Hand
                        , bid :: Int
                        } deriving (Show)

instance Eq HandBid where
    (==) :: HandBid -> HandBid -> Bool
    (HandBid h1 _) == (HandBid h2 _) = h1 == h2

instance Ord HandBid where
    compare :: HandBid -> HandBid -> Ordering
    (HandBid h1 _) `compare` (HandBid h2 _) = h1 `compare` h2

parseLine :: [Char] -> HandBid
parseLine s = HandBid (fromJust playingHand) b
    where   hb = words s
            cards = parseCards $ head hb
            playingHand = parseHand cards
            b = read (hb !! 1) :: Int

parseJLine :: [Char] -> HandBid
parseJLine s = HandBid (fromJust ph) b 
    where   hb = words s 
            cards = parseJCards $ head hb
            ph = parseJHand cards
            b = read (hb !! 1) :: Int 

parseHand :: [Strenth] -> Maybe Hand
parseHand cards
    | isFiveOfAKind cards = Just $ Hand FiveOfAKind cards
    | isFourOfAKind cards = Just $ Hand FourOfAKind cards
    | isFullHouse cards = Just $ Hand FullHouse cards
    | isThreeOfAKind cards = Just $ Hand ThreeOfAKind cards
    | isTwoPair cards = Just $ Hand TwoPair cards
    | isOnePair cards = Just $ Hand OnePair cards
    | isHighCard cards = Just $ Hand HighCard cards
    | otherwise = Nothing

parseJHand :: [Strenth] -> Maybe Hand
parseJHand cards
    | isJFiveOfAKind cards = Just $ Hand FiveOfAKind cards
    | isJFourOfAKind cards = Just $ Hand FourOfAKind cards
    | isJFullHouse cards = Just $ Hand FullHouse cards
    | isJThreeOfAKind cards = Just $ Hand ThreeOfAKind cards
    | isJTwoPair cards = Just $ Hand TwoPair cards
    | isJOnePair cards = Just $ Hand OnePair cards
    | isHighCard cards = Just $ Hand HighCard cards
    | otherwise = Nothing

parseCards :: [Char] -> [Strenth]
parseCards [] = []
parseCards ('A':xs) = Ace : parseCards xs
parseCards ('K':xs) = King : parseCards xs
parseCards ('Q':xs) = Queen : parseCards xs
parseCards ('J':xs) = Jack : parseCards xs
parseCards ('T':xs) = Ten : parseCards xs
parseCards ('9':xs) = Nine : parseCards xs
parseCards ('8':xs) = Eight : parseCards xs
parseCards ('7':xs) = Seven : parseCards xs
parseCards ('6':xs) = Six : parseCards xs
parseCards ('5':xs) = Five : parseCards xs
parseCards ('4':xs) = Four : parseCards xs
parseCards ('3':xs) = Three : parseCards xs
parseCards ('2':xs) = Two : parseCards xs
parseCards (_:xs) = parseCards xs

parseJCards :: [Char] -> [Strenth]
parseJCards [] = []
parseJCards ('A':xs) = Ace : parseJCards xs
parseJCards ('K':xs) = King : parseJCards xs
parseJCards ('Q':xs) = Queen : parseJCards xs
parseJCards ('J':xs) = Joker : parseJCards xs
parseJCards ('T':xs) = Ten : parseJCards xs
parseJCards ('9':xs) = Nine : parseJCards xs
parseJCards ('8':xs) = Eight : parseJCards xs
parseJCards ('7':xs) = Seven : parseJCards xs
parseJCards ('6':xs) = Six : parseJCards xs
parseJCards ('5':xs) = Five : parseJCards xs
parseJCards ('4':xs) = Four : parseJCards xs
parseJCards ('3':xs) = Three : parseJCards xs
parseJCards ('2':xs) = Two : parseJCards xs
parseJCards (_:xs) = parseJCards xs
