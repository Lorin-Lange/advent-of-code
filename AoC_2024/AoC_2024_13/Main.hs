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
parse = map parse' . chunksOf 4
    where parse' str = Machine {
        ax = p 12 a1, ay = p 3 a2, 
        bx = p 12 b1, by = p 3 b2, 
        px = p  9 p1, py = p 3 p2 }
            where p i s    = read $ drop i s
                  split n  = splitOn "," $ str !! n
                  [a1, a2] = split 0
                  [b1, b2] = split 1
                  [p1, p2] = split 2

solve :: [Machine] -> Int
solve = sum . map (\(x, y) -> 3 * x + y) . mapMaybe solve'
    where solve' m = if r == 0 then Just (x, y) else Nothing
            where (y, r) = (m.ay * m.px - m.ax * m.py) `divMod`
                           (m.ay * m.bx - m.ax * m.by)
                  x = (m.px - m.bx * y) `div` m.ax

unitConversion :: [Machine] -> [Machine]
unitConversion = map $ \m -> m { px = m.px + offset, py = m.py + offset }
    where offset = 10_000_000_000_000

main :: IO()
main = do lst <- parse . lines <$> readFile "input.txt"

          putStrLn $ "Part 1: " ++ show (solve lst)
          putStrLn $ "Part 2: " ++ show (solve $ unitConversion lst)
