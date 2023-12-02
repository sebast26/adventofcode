module Day01 (calibrationValue, realCalibrationValue) where

import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf, isSuffixOf)

-- Haskell Lists
-- if you have a list: abcdefgh, then
-- head | tail
--    a   bcdefg   h
--     init      | last
-- head is: a
-- tail is: bcdefgh
-- init is: abcdefg
-- tail is: h


firstDigit :: [Char] -> Int
firstDigit xs  
    | isDigit first = digitToInt first
    | otherwise = firstDigit $ tail xs
    where first = head xs

lastDigit :: [Char] -> Int
lastDigit xs  
    | isDigit end = digitToInt end
    | otherwise = lastDigit $ init xs 
    where end = last xs

calibrationValue :: [Char] -> Int
calibrationValue xs = 
    fd * 10 + ld
    where fd = firstDigit xs
          ld = lastDigit xs

realFirstDigit :: [Char] -> Int
realFirstDigit xs
    | isPrefixOf "one" xs = 1
    | isPrefixOf "two" xs = 2
    | isPrefixOf "three" xs = 3
    | isPrefixOf "four" xs = 4
    | isPrefixOf "five" xs = 5
    | isPrefixOf "six" xs = 6
    | isPrefixOf "seven" xs = 7
    | isPrefixOf "eight" xs = 8
    | isPrefixOf "nine" xs = 9
    | isDigit first = digitToInt first
    | otherwise = realFirstDigit $ tail xs
    where first = head xs

realLastDigit :: [Char] -> Int
realLastDigit xs
    | isSuffixOf "one" xs = 1
    | isSuffixOf "two" xs = 2
    | isSuffixOf "three" xs = 3
    | isSuffixOf "four" xs = 4
    | isSuffixOf "five" xs = 5
    | isSuffixOf "six" xs = 6
    | isSuffixOf "seven" xs = 7
    | isSuffixOf "eight" xs = 8
    | isSuffixOf "nine" xs = 9
    | isDigit end = digitToInt end
    | otherwise = realLastDigit $ init xs
    where end = last xs

realCalibrationValue :: [Char] -> Int
realCalibrationValue xs = 
    rf * 10 + rl
    where rf = realFirstDigit xs
          rl = realLastDigit xs