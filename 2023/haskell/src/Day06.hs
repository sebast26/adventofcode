module Day06 (race, simulations, wonSims) where

type Race = (Int, Int)

race :: (Int, Int) -> Race
race (x,y) = (x,y)

time :: Race -> Int
time (x, _) = x

distance :: Race -> Int
distance (_, x) = x

simulations :: Race -> [Int]
simulations r = m
    where   m = map (\speed -> do
                    let timeLeft = time r - speed
                    speed * timeLeft
                    ) [0..time r]

wonSims :: Race -> [Int] -> [Int]
wonSims r = filter (> distance r)