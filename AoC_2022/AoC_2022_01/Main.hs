----------------------------------------------------
--              Advent of Code 2022               --
--            Day 1: Calorie Counting             --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Text.Read ( readMaybe )
import Data.List ( sortBy )
import Data.List.Split ( endBy )
import Data.Maybe ( fromMaybe )
import Data.Ord ( Down(Down), comparing )

getSums :: [Maybe Integer] -> [Integer]
getSums = sortBy (comparing Down) . map (sum . map (fromMaybe 0)) . endBy [Nothing]

main :: IO()
main = do sums <- getSums . map readMaybe . lines <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (head sums)
          putStrLn $ "Part 2: " ++ show (sum $ take 3 sums)
