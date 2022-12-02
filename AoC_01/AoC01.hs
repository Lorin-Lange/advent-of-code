----------------------------------------------------
--                                                --
--              Advent of Code 2022               --
--            Day 1: Calorie Counting             --
--            Solution by Lorin Lange             --
--                                                --
----------------------------------------------------

module AoC01 where

import Text.Read ( readMaybe )
import Data.List ( sort )
import Data.List.Split ( endBy )
import Data.Maybe ( fromMaybe )

getInput :: IO [Maybe Integer]
getInput = do lst <- lines <$> readFile "./input.txt"
              return $ map readMaybe lst

sum' :: [Maybe Integer] -> [Integer]
sum' lst = reverse . sort . map (sum . map (fromMaybe 0)) $ endBy [Nothing] lst

getMaxElf :: [Integer] -> Integer
getMaxElf = head

getTopThreeElvesSum :: [Integer] -> Integer
getTopThreeElvesSum = sum . take 3

main :: IO()
main = do
    lst <- getInput
    let sums = sum' lst
    print $ getMaxElf sums
    print $ getTopThreeElvesSum sums