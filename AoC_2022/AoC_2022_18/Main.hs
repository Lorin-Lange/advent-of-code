----------------------------------------------------
--              Advent of Code 2022               --
--            Day 18: Boiling Boulders            --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.List.Split ( splitOn )

adjacent :: [(Int, Int, Int)] -> Int
adjacent []     = 0
adjacent (x:xs) = length (filter (isAdjacent x) xs) + adjacent xs

isAdjacent :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
isAdjacent (x1, y1, z1) (x2, y2, z2)
    =  (x1 == x2 && y1 == y2 && (z1 == z2 - 1 || z1 == z2 + 1))
    || (y1 == y2 && z1 == z2 && (x1 == x2 - 1 || x1 == x2 + 1))
    || (z1 == z2 && x1 == x2 && (y1 == y2 - 1 || y1 == y2 + 1))

totalSurface :: [(Int, Int, Int)] -> Int
totalSurface lst = length lst * 6 - adjacent lst * 2

trappedSurface :: [(Int, Int, Int)] -> Int
trappedSurface lst = undefined


parse :: String -> (Int, Int, Int)
parse str = (read $ head lst, read $ lst !! 1, read $ lst !! 2)
    where lst = splitOn "," str

main :: IO ()
main = do
    inp <- map parse . lines <$> readFile "test_input.txt"

    let surface = totalSurface inp
    putStrLn $ "Part 1: " ++ show surface
    putStrLn $ "Part 2: " ++ show (surface - trappedSurface inp)
