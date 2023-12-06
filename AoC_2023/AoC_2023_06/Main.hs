----------------------------------------------------
--              Advent of Code 2023               --
--               Day 6: Wait For It               --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.List.Split (splitOn)

parseRaces :: [String] -> [(Int, Int)]
parseRaces [s1, s2] = zip (getList s1) (getList s2)
    where getList lst = map read $ words $ splitOn ":" lst !! 1

parseRace :: [String] -> (Int, Int)
parseRace [s1, s2] = (getNum s1, getNum s2)
    where getNum s = read $ filter (/= ' ') $ splitOn ":" s !! 1

numberOfWays :: (Int, Int) -> Int
numberOfWays i@(t, _) = length $ filter (possible i) [1..t-1]
    where possible (t, d) pt = pt * (t - pt) > d

main :: IO()
main = do
    file <- readFile "input.txt"
    let input = lines file

    let races = parseRaces input
    let firstPart = product $ map numberOfWays races
    print firstPart

    let race = parseRace input
    let secondPart = numberOfWays race
    print secondPart