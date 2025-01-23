----------------------------------------------------
--              Advent of Code 2024               --
--             Day 25: Code Chronicle             --
--            Solution by Lorin Lange             --
----------------------------------------------------

{-# Language LambdaCase #-}

module Main where

import Data.List.Split ( splitOn )
import Data.List ( partition, transpose )

parse :: ([[String]], [[String]]) -> ([[Int]], [[Int]])
parse (l1, l2) = (applyCount l1, applyCount $ map reverse l2)
    where applyCount = map $ map ((\n -> n - 1) . count) . transpose
          count = \case [] -> 0; ('.':_) -> 0; ('#':xs) -> 1 + count xs

schematics :: [String] -> ([[String]], [[String]])
schematics = partition arePins . splitOn [""]
    where arePins = all (== '#') . head

fit :: ([Int], [Int]) -> Bool
fit (l1, l2) = all (<= 5) $ zipWith (+) l1 l2

makeComb :: ([[Int]], [[Int]]) -> [([Int], [Int])]
makeComb (l1, l2) = l1 >>= \l1' -> l2 >>= \l2' -> pure (l1', l2')

main :: IO()
main = do combi <- makeComb . parse . schematics . lines <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (length $ filter fit combi)
