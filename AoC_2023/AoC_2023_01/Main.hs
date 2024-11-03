----------------------------------------------------
--              Advent of Code 2023               --
--               Day 1: Trebuchet?!               --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.Char ( isDigit )
import Data.List ( isPrefixOf )

getNumbers :: String -> Int
getNumbers inp = read [head nums, last nums]
    where nums = filter isDigit inp

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
    input <- lines <$> readFile "input.txt"

    let part1 = sum $ map getNumbers input
    putStrLn $ "Part 1: " ++ show part1

    let part2 = sum $ map (getNumbers . substitute) input
    putStrLn $ "Part 2: " ++ show part2
