----------------------------------------------------
--              Advent of Code 2024               --
--            Day 2: Red-Nosed Reports            --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.List ( inits, tails )

isSafe :: [Int] -> Bool
isSafe lst = monotonic && inRange
    where deltas    = zipWith (-) (init lst) (tail lst)
          monotonic = all (> 0) deltas || all (< 0) deltas
          inRange   = all ((3 >=) . abs) deltas

isTolerated :: [Int] -> Bool
isTolerated lst = any isSafe lsts
    where lsts = zipWith (++) (inits lst) (map (drop 1) $ tails lst)

solve :: ([Int] -> Bool) -> [[Int]] -> Int
solve p = length . filter p

main :: IO()
main = do lst <- map (map read . words) . lines <$> readFile "input.txt"

          putStrLn $ "Part 1: " ++ show (solve isSafe lst)
          putStrLn $ "Part 2: " ++ show (solve isTolerated lst)
