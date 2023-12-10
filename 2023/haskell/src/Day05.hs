{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Day05 (readSeeds, readMapEntry, almanac, seedToLocation, rangesOfSeeds, seedsFromRanges) where

data Almanac = Almanac  { seeds :: [Int]
                        , seedToSoil :: [(Int, Int, Int)]
                        , soilToFertilizer :: [(Int, Int, Int)]
                        , fertilizerToWater :: [(Int, Int, Int)]
                        , waterToLight :: [(Int, Int, Int)]
                        , lightToTemperature :: [(Int, Int, Int)]
                        , temperatureToHumidity :: [(Int, Int, Int)]
                        , humidityToLocation :: [(Int, Int, Int)]
                        } deriving (Show)

almanac :: [Int] -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> Almanac
almanac = Almanac

seedToLocation :: Almanac -> Int -> Int
seedToLocation alm seed = findMapping (humidityToLocation alm) hum
    where   soil = findMapping (seedToSoil alm) seed
            fert = findMapping (soilToFertilizer alm) soil
            water = findMapping (fertilizerToWater alm) fert
            light = findMapping (waterToLight alm) water
            temp = findMapping (lightToTemperature alm) light
            hum = findMapping (temperatureToHumidity alm)  temp

dstRangeStart :: (a, b, c) -> a
dstRangeStart (x, _, _) = x

srcRangeStart :: (a, b, c) -> b
srcRangeStart (_, x, _) = x

rangeLength :: (a, b, c) -> c
rangeLength (_, _, x) = x

readSeeds :: String -> [Int]
readSeeds str = map (\x -> read x :: Int) $ words str

readMapEntry :: [Char] -> (Int, Int, Int)
readMapEntry str = (nums !! 0, nums !! 1, nums !! 2)
    where nums = map (\x -> read x :: Int) $ words str

findMapping :: [(Int, Int, Int)] -> Int -> Int
findMapping mapEntries key = if valuesFiltered /= [] then head valuesFiltered else key
    where   values = map (mapping key) mapEntries 
            valuesFiltered = filter (/= key) values 
            
mapping :: Int -> (Int, Int, Int) -> Int
mapping i mapEntry
    | srcStart <= i && i < srcStart + rangeLen = dstStart + diff
    | otherwise = i
    where   srcStart = srcRangeStart mapEntry
            rangeLen = rangeLength mapEntry
            dstStart = dstRangeStart mapEntry
            diff = i - srcStart

rangesOfSeeds :: Num b => [b] -> [(b, b)]
rangesOfSeeds [] = []
rangesOfSeeds [_] = []
rangesOfSeeds (x:y:xs) = (x, x+y) : rangesOfSeeds xs 

seedsFromRanges :: Enum a => [(a, a)] -> [[a]]
seedsFromRanges [] = []
seedsFromRanges ((s, e):xs) = [s..e] : seedsFromRanges xs