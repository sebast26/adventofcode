{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Day05 (readSeeds, readMapEntry, almanac, seedToLocation) where

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
seedToLocation alm seed = findMapping alm humidityToLocation hum
    where   soil = findMapping alm seedToSoil seed
            fert = findMapping alm soilToFertilizer soil
            water = findMapping alm fertilizerToWater fert
            light = findMapping alm waterToLight water
            temp = findMapping alm lightToTemperature light
            hum = findMapping alm temperatureToHumidity temp

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

findMapping :: Almanac -> (Almanac -> [(Int, Int, Int)]) -> Int -> Int
findMapping alm fn key = if valuesFiltered /= [] then head valuesFiltered else key
    where   values = map (mapping key) $ fn alm
            valuesFiltered = filter (/= key) values 
            
mapping :: Int -> (Int, Int, Int) -> Int
mapping i mapEntry
    | srcStart <= i && i < srcStart + rangeLen = dstStart + diff
    | otherwise = i
    where   srcStart = srcRangeStart mapEntry
            rangeLen = rangeLength mapEntry
            dstStart = dstRangeStart mapEntry
            diff = i - srcStart
