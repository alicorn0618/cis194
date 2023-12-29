module Golf where

import Data.List (transpose)

-- skips implemented with higher order functions
-- map, zip, filter, take, drop, length
getKthElements :: [a] -> Int -> [a]
getKthElements xs k = map snd $ filter (\(i, _) -> i `mod` k == 0) $ zip [1 ..] xs

skips :: [a] -> [[a]]
skips xs = map (getKthElements xs) [1 .. length xs]

-- localMaxima implemented with higher order functions
localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(_, y, _) -> y) $ filter (\(x, y, z) -> y > x && y > z) $ zip3 xs (tail xs) $ tail $ tail xs

--
histogram :: [Integer] -> String
histogram xs = unlines $ map (map (\x -> if x == 0 then ' ' else '*')) $ reverse $ transpose $ map (\x -> map (\y -> if y == x then 1 else 0) xs) [0 .. 9]