module Day01 (calibrationValue) where

import Data.Char (isDigit, digitToInt)

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