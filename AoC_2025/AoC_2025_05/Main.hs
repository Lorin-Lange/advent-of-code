----------------------------------------------------
--              Advent of Code 2025               --
--                Day 5: Cafeteria                --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main (main) where

import Data.List.Split (splitOn)
import Data.Ix (Ix(inRange))
import Data.List (foldl', sortOn)

parseDB :: String -> ([(Int, Int)], [Int])
parseDB input = (parsedRanges, parsedIds)
   where (rawRanges, _ : rawIds) = break null $ lines input
         parse         = map $ toPair . splitOn "-"
         toPair [s, e] = (read s, read e)
         parsedIds     = map read rawIds
         parsedRanges  = sortOn fst $ parse rawRanges

part1 :: [Int] -> [(Int, Int)] -> Int
part1 ids ranges = sum [ 1 | id <- ids, any (`inRange` id) ranges ]

part2 :: [(Int, Int)] -> Int
part2 = fst . foldl' go (0, 0)
  where go (t, le) (s, e) | le >= e   = (t, le)
                          | le < s    = (t + e - s + 1, e)
                          | otherwise = (t + e - le, e)

main :: IO()
main = do (ranges, ids) <- parseDB <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (part1 ids ranges)
          putStrLn $ "Part 2: " ++ show (part2 ranges)
