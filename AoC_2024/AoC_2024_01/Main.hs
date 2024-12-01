----------------------------------------------------
--              Advent of Code 2024               --
--           Day 1: Historian Hysteria            --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.List ( sort, group )
import Data.Maybe ( fromMaybe )

makeScore :: [(Int, Int)] -> Int -> Int
makeScore scores n = fromMaybe 0 (lookup n scores) * n

parse :: String -> (Int, Int)
parse str = (read $ head lst, read $ lst !! 1)
    where lst = words str

main :: IO()
main = do
    lst <- map parse . lines <$> readFile "input.txt"
    let lst1 = sort $ map fst lst
    let lst2 = sort $ map snd lst

    putStrLn $ "Part 1: " ++ show (sum $ zipWith (\a b -> abs $ a - b) lst1 lst2)

    let freq = map (\l -> (head l, length l)) $ group lst2
    putStrLn $ "Part 2: " ++ show (sum $ map (makeScore freq) lst1)
