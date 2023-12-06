----------------------------------------------------
--              Advent of Code 2023               --
--             Day 2: Cube Conundrum              --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.List.Split (splitOn)

type Game = (Int, [[(String, Int)]])

parseGame :: String -> Game
parseGame g =
    let inp  = splitOn ":" g
        id   = read $ drop 5 (head inp)
        lsts = map (splitOn ",") $ splitOn ";" (inp !! 1)
    in (id, map parserH lsts)
    where parserH = map (\s -> let r = splitOn " " s 
                               in (r !! 2, read $ r !! 1))

condition :: Game -> Bool
condition (_, lst) = all (all (\t -> 
    case t of
        ("red",   n) -> n <= 12
        ("green", n) -> n <= 13
        ("blue",  n) -> n <= 14
     )) lst 

getProduct :: Game -> Int
getProduct (_, lst) = product $ map getMax ["red", "blue", "green"]
    where getMax c = snd . maximum $ concatMap (filter $ \(s, _) -> s == c) lst

main :: IO()
main = do
    file <- readFile "input.txt"
    let games = map parseGame $ lines file

    let partOne = sum $ map (\(id, _) -> id) $ filter condition games
    print partOne

    let partTwo = sum $ map getProduct games
    print partTwo