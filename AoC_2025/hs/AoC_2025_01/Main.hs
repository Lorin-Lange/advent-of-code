----------------------------------------------------
--              Advent of Code 2025               --
--             Day 1: Secret Entrance             --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main(main) where

import Data.List (mapAccumL)

parse :: String -> Int
parse ('L':n) = negate $ read n
parse ('R':n) = read n

rot :: Int -> Int -> Int
rot n m = (n + m) `mod` 100

atZero :: Int -> Int -> Int
atZero n m | m > 0     = (n + m) `div` 100
           | otherwise = (n + m) `div` (-100) - n `div` (-100)

part1 :: [Int] -> Int
part1 lst = sum [1 | n <- scanl rot 50 lst, n == 0]

part2 :: [Int] -> Int
part2 = sum . snd . mapAccumL step 50
  where step n m = (rot n m, atZero n m)

main :: IO()
main = do input <- map parse . lines <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (part1 input)
          putStrLn $ "Part 2: " ++ show (part2 input)
