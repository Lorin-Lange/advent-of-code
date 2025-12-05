----------------------------------------------------
--              Advent of Code 2025               --
--                Day 5: Cafeteria                --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main (main) where

import Data.List.Split (splitOn)
import Data.Ix (Ix(inRange))
import Data.List (foldl', sortOn)

parse :: String -> ([(Int, Int)], [Int])
parse input = (sortOn fst $ toPairs ranges, map read ids)
   where (ranges, _:ids) = break null $ lines input
         toPairs = map $ (\[s, e] -> (read s, read e)) . splitOn "-"

part1 :: [Int] -> [(Int, Int)] -> Int
part1 ids ranges = sum [ 1 | id <- ids, any (`inRange` id) ranges ]

part2 :: [(Int, Int)] -> Int
part2 = fst . foldl' go (0, 0)
  where go (t, le) (s, e) | le >= e   = (t, le)
                          | le < s    = (t + e - s + 1, e)
                          | otherwise = (t + e - le, e)

main :: IO()
main = do (ranges, ids) <- parse <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (part1 ids ranges)
          putStrLn $ "Part 2: " ++ show (part2 ranges)
