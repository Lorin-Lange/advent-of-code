----------------------------------------------------
--              Advent of Code 2023               --
--              Day 4: Scratchcards               --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import qualified Data.Set as Set
import Data.List.Split (splitOn)

type Card = (Int, [Int], Set.Set Int)

parseCard :: String -> Card
parseCard s =
    let inp   = splitOn ":" s
        id    = read $ drop 5 (head inp)
        cards = splitOn "|" (inp !! 1)
        lst   = map read $ filter (/= "") $ splitOn " " (head cards)
        set   = map read $ filter (/= "") $ splitOn " " (cards !! 1)
    in (id, lst, Set.fromList set)

calculate :: Card -> Int
calculate (_, lst, set) = 
    let n = length $ calcH lst set
    in if n == 0 then 0 else 2^(n-1)
    where 
        calcH []     s = []
        calcH (x:xs) s | Set.member x s = x : calcH xs s
                       | otherwise      =     calcH xs s

main :: IO()
main = do
    file <- readFile "test_input_1.txt"
    let input = lines file
    let cards = map parseCard input

    let partOne = sum $ map calculate cards
    print partOne