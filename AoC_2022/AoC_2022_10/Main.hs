----------------------------------------------------
--              Advent of Code 2022               --
--            Day 10: Cathode-Ray Tube            --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.List ( lookup, intercalate )
import Data.Maybe ( fromJust )
import Data.List.Split (chunksOf)

data Data = Noop | Addx Int
    deriving Show

parseInput :: [String] -> [Data]
parseInput = concatMap $ \s -> if s == "noop"
    then [Noop]
    else [Noop, Addx $ read $ drop 5 s]

apply :: [Data] -> [(Int, Int)]
apply = scanl f (1, 1) . zip [2..]
    where f (_, v) (i, Noop)   = (i, v)
          f (_, v) (i, Addx n) = (i, v + n)

pixel :: (Int, Int) -> Char
pixel (c, x) | pos `elem` sprite = '#'
             | otherwise         = '.'
    where pos    = (c - 1) `rem` 40
          sprite = [x - 1, x, x + 1]

main :: IO ()
main = do
    lst <- apply . parseInput . lines <$> readFile "input.txt"

    let res1 = sum $ map (\n -> n * fromJust (lookup n lst)) [20, 60..220]
    putStrLn $ "Part 1: " ++ show res1

    let res2 = intercalate "\n" . chunksOf 40 . map pixel $ init lst
    putStrLn $ "Part 2:\n" ++ res2
