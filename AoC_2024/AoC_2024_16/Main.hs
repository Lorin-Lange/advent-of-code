----------------------------------------------------
--              Advent of Code 2024               --
--             Day 16: Reindeer Maze              --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import qualified Data.Map as Map




makeGrid :: (Char -> a) -> [String] -> Map.Map (Int, Int) a
makeGrid convert = Map.fromList . concatMap (\(y, l) -> zipWith (\ch x -> ((x, y), convert ch)) l [0..]) . zip [0..]

main :: IO()
main = do input <- lines <$> readFile "test_input.txt"
          print input

          putStrLn $ "Part 1: " ++ show "TODO"
          putStrLn $ "Part 2: " ++ show "TODO"


