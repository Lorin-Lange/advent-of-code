----------------------------------------------------
--              Advent of Code 2024               --
--               Day 5: Print Queue               --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.List.Split ( splitOn )
import Data.List ( elemIndex, partition, sortBy )
import Data.Maybe ( fromJust )

parseInput :: [String] -> ([(Int, Int)], [[Int]])
parseInput lst' = (rules, updates) where
    lst     = splitOn [""] lst'
    rules   = map ((\[v1, v2] -> (read v1, read v2)) . splitOn "|") $ head lst
    updates = map (map read .  splitOn ",") $ lst !! 1

isCorrect :: [(Int, Int)] -> [Int] -> Bool
isCorrect updates rules = not $ any violatesRule updates
    where violatesRule (a, b) = a `elem` rules && b `elem` rules &&
            fromJust (elemIndex a rules) > fromJust (elemIndex b rules)

getMiddle :: [a] -> a
getMiddle lst = lst !! (length lst `div` 2)

order :: [(Int, Int)] -> [Int] -> [Int]
order orders = sortBy (\x y -> if (x, y) `elem` orders then LT else GT)

main :: IO()
main = do (rules, updates) <- parseInput . lines <$> readFile "input.txt"
          let (correct, incorrect) = partition (isCorrect rules) updates

          putStrLn $ "Part 1: " ++ show (sum $ map getMiddle correct)
          putStrLn $ "Part 2: " ++ show (sum $ map (getMiddle . order rules) incorrect)
