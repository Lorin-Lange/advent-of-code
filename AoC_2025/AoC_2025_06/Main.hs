----------------------------------------------------
--              Advent of Code 2025               --
--             Day 6: Trash Compactor             --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main (main) where

import Data.List (transpose)
import Data.Char (isSpace)
import Data.List.Split (splitWhen)

parseOp :: Num a => String -> a -> a -> a
parseOp "*" = (*)
parseOp "+" = (+)

part1 :: [String] -> Int
part1 inp = sum $ zipWith foldl1 ops nums
    where inp' = transpose $ map words inp
          nums = map (map read . init) inp'
          ops  = map (parseOp . last) inp'

part2 :: [String] -> Int
part2 inp = sum $ zipWith foldl1 ops nums
    where inp' = chunkByEmpty . reverse $ transpose inp
          nums = map (map (read . init)) inp'
          ops  = map (parseOp . (:[]) . last . last) inp'

chunkByEmpty :: [String] -> [[String]]
chunkByEmpty = filter (not . null) . splitWhen (all isSpace)

main :: IO()
main = do input <- lines <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (part1 input)
          putStrLn $ "Part 2: " ++ show (part2 input)
