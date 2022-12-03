----------------------------------------------------
--                                                --
--              Advent of Code 2022               --
--         Day 3: Rucksack Reorganization         --
--            Solution by Lorin Lange             --
--                                                --
----------------------------------------------------

module AoC03 where

import Data.Maybe ( fromMaybe )
import Data.List.Split ( chunksOf )

priorities :: [(Char, Integer)]
priorities = zip ['a'..'z'] [1..26] ++ zip ['A'..'Z'] [27..52]

getInput :: IO [String]
getInput = lines <$> readFile "./input.txt"

chunk :: String -> (String, String)
chunk str = splitAt (length str `div` 2) str

testInput :: String
testInput = "vJrwpWtwJgWrhcsFMMfFFhFp\n\
            \jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
            \PmmdzqPrVvPwwTWBwg\n\
            \wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
            \ttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"

findDuplicate :: (String, String) -> Char
findDuplicate ([],   str) = error "should not happen"
findDuplicate (x:xs, str) | x `elem` str = x 
                          | otherwise    = findDuplicate (xs, str)

findCommon :: [String] -> Char
findCommon [[],   s2, s3] = error "should not happen"
findCommon [x:xs, s2, s3] | x `elem` s2 && x `elem` s3 = x
                          | otherwise                  = findCommon [xs, s2, s3]

main :: IO()
main = do
    print "Result of part one"
    let testInputSum = sum $ map (\i -> fromMaybe 0 $ lookup (findDuplicate $ chunk i) priorities) $ lines testInput
    print testInputSum

    input <- getInput
    let inputSum = sum $ map (\i -> fromMaybe 0 $ lookup (findDuplicate $ chunk i) priorities) input
    print inputSum

    print "Result of part two"
    let inp = map findCommon $ chunksOf 3 $ lines testInput
    let res = sum $ map (\i -> fromMaybe 0 $ lookup i priorities) inp
    print res

    let inp = map findCommon $ chunksOf 3 input
    let res = sum $ map (\i -> fromMaybe 0 $ lookup i priorities) inp
    print res