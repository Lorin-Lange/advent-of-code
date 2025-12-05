----------------------------------------------------
--              Advent of Code 2025               --
--                Day 5: Cafeteria                --
--            Solution by Lorin Lange             --
----------------------------------------------------

{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Data.List.Split (splitOn)
import Data.Ix (Ix(inRange))
import Data.List (foldl', sortOn)

data DB = DB
  { ranges :: [(Int, Int)]
  , ids    :: [Int]
  } deriving Show

parseDB :: String -> DB
parseDB input = DB { ranges = parsedRanges, ids = parsedIds }
   where (rawRanges, _ : rawIds) = break null $ lines input
         parse         = map $ toPair . splitOn "-"
         toPair [s, e] = (read s, read e)
         parsedIds     = map read rawIds
         parsedRanges  = sortOn fst $ parse rawRanges

part1 :: DB -> Int
part1 db = sum [ 1 | id <- db.ids, any (`inRange` id) db.ranges ]

part2 :: DB -> Int
part2 db = fst $ foldl' go (0, 0) db.ranges
  where go (t, le) (s, e) | le >= e   = (t, le)
                          | le < s    = (t + e - s + 1, e)
                          | otherwise = (t + e - le, e)

main :: IO()
main = do db <- parseDB <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (part1 db)
          putStrLn $ "Part 2: " ++ show (part2 db)
