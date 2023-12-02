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

parserH :: [String] -> [(String, Int)]
parserH = map (\s -> let r = splitOn " " s 
                     in (r !! 2, read $ r !! 1))

condition :: Game -> Bool
condition (_, lst) = all (all (\t -> 
    case t of
        ("red", n)   -> n <= 12
        ("green", n) -> n <= 13
        ("blue", n)  -> n <= 14
     )) lst 

getProduct :: Game -> Int
getProduct (_, lst) =
    let (_, red)   = getMax lst "red"
        (_, blue)  = getMax lst "blue"
        (_, green) = getMax lst "green"
    in red * blue * green

getMax :: [[(String, Int)]] -> String -> (String, Int)
getMax lst color = 
    maximum $ concatMap (\l -> filter (\(s,_) -> s == color) l) lst

main :: IO()
main = do
    file <- readFile "input.txt"
    let input = lines file
    let games = map parseGame input
    
    let partOne = sum $ map (\(id, _) -> id) $ filter condition games
    print partOne

    let partTwo = sum $ map getProduct games
    print partTwo