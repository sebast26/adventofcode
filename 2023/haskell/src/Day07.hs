{-# LANGUAGE InstanceSigs #-}
module Day07 where

import Data.List (nub, sort, group)
import Data.Maybe (fromJust)

data Strenth = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord)

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

isFourOfAKind :: Ord a => [a] -> Bool
isFourOfAKind s = length groups == 2 && 1 `elem` sizes && 4 `elem` sizes
    where   groups = group $ sort s
            sizes = map length groups

isFullHouse :: Ord a => [a] -> Bool
isFullHouse s = length groups == 2 && 2 `elem` sizes && 3 `elem` sizes
    where   groups = group $ sort s
            sizes = map length groups

isThreeOfAKind :: Ord a => [a] -> Bool
isThreeOfAKind s = length groups >= 2 && 3 `elem` sizes
    where   groups = group $ sort s
            sizes = map length groups

isTwoPair :: Ord a => [a] -> Bool
isTwoPair s = length groups == 3 && sortSizes == [1,2,2]
    where   groups = group $ sort s
            sizes = map length groups
            sortSizes = sort sizes

isOnePair :: Ord a => [a] -> Bool
isOnePair s = length groups == 4 && sortSized == [1, 1, 1, 2]
    where   groups = group $ sort s
            sizes = map length groups
            sortSized = sort sizes

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