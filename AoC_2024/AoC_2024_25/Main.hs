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
parse lst = (count l, count $ map reverse k)
    where count  = map $ map count' . transpose
          count' = \case [] -> -1; ('.':_) -> -1; ('#':t) -> 1 + count' t
          (l, k) = partition (all (== '#') . head) $ splitOn [""] lst

fit :: ([Int], [Int]) -> Bool
fit = all (<= 5) . uncurry (zipWith (+))

combi :: ([[Int]], [[Int]]) -> [([Int], [Int])]
combi (l1, l2) = l1 >>= \l1' -> l2 >>= \l2' -> [(l1', l2')]

main :: IO()
main = do cs <- combi . parse . lines <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (length $ filter fit cs)
