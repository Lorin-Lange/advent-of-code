----------------------------------------------------
--              Advent of Code 2024               --
--           Day 1: Historian Hysteria            --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.List.Split ( endBy )
import Data.List ( sort, group )
import Data.Maybe ( fromMaybe )

makeScore :: [(Int, Int)] -> Int -> Int
makeScore scores n = fromMaybe 0 (lookup n scores) * n

parse :: String -> (Int, Int)
parse str = let lst = endBy "   " str
            in (read $ head lst, read $ lst !! 1)

main :: IO()
main = do
    input <- map parse . lines <$> readFile "test_input.txt"
    let lst1 = sort $ map fst input
    let lst2 = sort $ map snd input

    putStrLn $ "Part 1: " ++ show (sum $ zipWith (\a b -> abs $ a - b) lst1 lst2)

    let freq = map (\lst -> (head lst, length lst)) $ group lst2
    putStrLn $ "Part 2: " ++ show (sum $ map (makeScore freq) lst1)
