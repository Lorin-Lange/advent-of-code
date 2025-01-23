----------------------------------------------------
--              Advent of Code 2024               --
--             Day 25: Code Chronicle             --
--            Solution by Lorin Lange             --
----------------------------------------------------

{-# Language LambdaCase #-}

module Main where

import Data.List.Split ( splitOn )
import Data.List ( partition, transpose )

parse :: [String] -> ([[Int]], [[Int]])
parse lst = (count l1, count $ map reverse l2)
    where (l1, l2) = partition (all (== '#') . head) $ splitOn [""] lst
          count = map $ map count' . transpose
          count' = \case [] -> -1; ('.':_) -> -1; ('#':xs) -> 1 + count' xs

fit :: ([Int], [Int]) -> Bool
fit (l1, l2) = all (<= 5) $ zipWith (+) l1 l2

makeComb :: ([[Int]], [[Int]]) -> [([Int], [Int])]
makeComb (l1, l2) = l1 >>= \l1' -> l2 >>= \l2' -> [(l1', l2')]

main :: IO()
main = do combi <- makeComb . parse . lines <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (length $ filter fit combi)
