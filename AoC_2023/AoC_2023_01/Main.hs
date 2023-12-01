----------------------------------------------------
--              Advent of Code 2023               --
--               Day 1: Trebuchet?!               --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.Char (isDigit)
import Data.List (isPrefixOf)

getNumbers :: String -> Int
getNumbers inp = 
    let nums = filter isDigit inp
    in read [head nums, last nums]

substitute :: String -> String 
substitute ""         = ""
substitute str@(x:xs) | "one"   `isPrefixOf` str = '1' : substitute xs
                      | "two"   `isPrefixOf` str = '2' : substitute xs
                      | "three" `isPrefixOf` str = '3' : substitute xs
                      | "four"  `isPrefixOf` str = '4' : substitute xs
                      | "five"  `isPrefixOf` str = '5' : substitute xs
                      | "six"   `isPrefixOf` str = '6' : substitute xs
                      | "seven" `isPrefixOf` str = '7' : substitute xs
                      | "eight" `isPrefixOf` str = '8' : substitute xs
                      | "nine"  `isPrefixOf` str = '9' : substitute xs
                      | otherwise                =  x  : substitute xs

main :: IO()
main = do
    file <- readFile "input.txt"
    let input = lines file

    let part1 = sum $ map getNumbers input
    print part1

    let part2 = sum $ map (getNumbers . substitute) input
    print part2