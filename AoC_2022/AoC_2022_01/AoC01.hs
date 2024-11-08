----------------------------------------------------
--              Advent of Code 2022               --
--            Day 1: Calorie Counting             --
--            Solution by Lorin Lange             --
----------------------------------------------------

module AoC01 where

import Text.Read ( readMaybe )
import Data.List ( sortBy )
import Data.List.Split ( endBy )
import Data.Maybe ( fromMaybe )
import Data.Ord ( Down(Down), comparing ) 

getInput :: IO [Maybe Integer]
getInput = do lst <- lines <$> readFile "input.txt"
              return $ map readMaybe lst

getSums :: [Maybe Integer] -> [Integer]
getSums = sortBy (comparing Down) . map (sum . map (fromMaybe 0)) . endBy [Nothing]

main :: IO()
main = do
    sums <- getSums <$> getInput
    putStrLn $ "Part 1: " ++ show (head sums)         --  68442
    putStrLn $ "Part 2: " ++ show (sum $ take 3 sums) -- 204837