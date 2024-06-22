----------------------------------------------------
--              Advent of Code 2022               --
--            Day 1: Calorie Counting             --
--            Solution by Lorin Lange             --
----------------------------------------------------

module AoC01 where

import Text.Read ( readMaybe )
import Data.List ( sort )
import Data.List.Split ( endBy )
import Data.Maybe ( fromMaybe )

getInput :: IO [Maybe Integer]
getInput = do lst <- lines <$> readFile "input.txt"
              return $ map readMaybe lst

getSums :: [Maybe Integer] -> [Integer]
getSums = reverse . sort . map (sum . map (fromMaybe 0)) . endBy [Nothing]

main :: IO()
main = do
    lst <- getInput
    let sums = getSums lst
    putStrLn $ "Part 1: " ++ show (head sums)
    putStrLn $ "Part 2: " ++ show (sum $ take 3 sums)