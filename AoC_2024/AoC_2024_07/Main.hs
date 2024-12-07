----------------------------------------------------
--              Advent of Code 2024               --
--              Day 7: Bridge Repair              --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.List.Split ( splitOn )

parse :: String -> (Integer, [Integer])
parse str = (read value, nums)
    where [value, lst] = splitOn ":" str
          nums = map read $ splitOn " " $ tail lst

isTestValue :: (Integer -> Integer -> [Integer]) -> (Integer, [Integer]) -> Bool
isTestValue operators (expected, nums) = expected `elem` makeNums [-1] nums
    where makeNums = foldl $ \is x -> concatMap (`operators` x) is

main :: IO()
main = do input <- map parse . lines <$> readFile "input.txt"

          let f1 n x = if n == -1 then [x] else [n * x, n + x]
          putStrLn $ "Part 1: " ++ show (sum . map fst $ filter (isTestValue f1) input)

          let f2 n x = if n == -1 then [x] else [n * x, n + x, read $ show n ++ show x]
          putStrLn $ "Part 2: " ++ show (sum . map fst $ filter (isTestValue f2) input)
