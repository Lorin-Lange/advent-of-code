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

calcH :: [Int] -> Set.Set Int -> [Int]
calcH []     s = []
calcH (x:xs) s | Set.member x s = x : calcH xs s
               | otherwise      =     calcH xs s

calculate2 :: [Card] -> [(Int, Int, Int)]
calculate2 = map (\(id, lst, set) -> (id, length $ calcH lst set, 1))

calc :: [(Int, Int, Int)] -> [(Int, Int, Int)]
calc []                 = []
calc (tu@(_, n, m):xs) =
    let next = map (\(id, no, t) -> (id, no, t + m)) $ take n xs
        rest = drop n xs
    in tu : calc (next ++ rest)

main :: IO()
main = do
    file <- readFile "input.txt"
    let input = lines file
    let cards = map parseCard input

    let partOne = sum $ map calculate cards
    print partOne

    let partTwo = sum $ map (\(_, _, t) -> t) $ calc $ calculate2 cards
    print partTwo