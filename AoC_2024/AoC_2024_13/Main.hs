----------------------------------------------------
--              Advent of Code 2024               --
--            Day 13: Claw Contraption            --
--            Solution by Lorin Lange             --
----------------------------------------------------

{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Data.List.Split ( chunksOf, splitOn )
import Data.Maybe ( mapMaybe )

data Machine = Machine 
    { ax, ay :: Int
    , bx, by :: Int
    , px, py :: Int }

parse :: [String] -> [Machine]
parse inp = map parseH $ chunksOf 4 inp 
    where parseH str = Machine { 
        ax = read $ drop 12 a1, ay = read $ drop 3 a2,
        bx = read $ drop 12 b1, by = read $ drop 3 b2,
        px = read $ drop  9 p1, py = read $ drop 3 p2 }
            where [a1, a2] = splitOn "," $ head str
                  [b1, b2] = splitOn "," $ str !! 1
                  [p1, p2] = splitOn "," $ str !! 2

solve :: [Machine] -> Int
solve = sum . map (\(x, y) -> 3 * x + y) . mapMaybe solve'
    where solve' m = if r == 0 then Just (x, y) else Nothing
            where (y, r) = (m.ay * m.px - m.ax * m.py) `divMod`
                           (m.ay * m.bx - m.ax * m.by)
                  x = (m.px - m.bx * y) `div` m.ax

unitConversion :: [Machine] -> [Machine]
unitConversion = map $ \m -> m { px = m.px + offset, py = m.py + offset }
    where offset = 10000000000000

main :: IO()
main = do lst <- parse . lines <$> readFile "input.txt"

          putStrLn $ "Part 1: " ++ show (solve lst)
          putStrLn $ "Part 2: " ++ show (solve $ unitConversion lst)
