----------------------------------------------------
--              Advent of Code 2024               --
--             Day 15: Warehouse Woes             --
--            Solution by Lorin Lange             --
----------------------------------------------------

{-# Language LambdaCase #-}

module Main where

import Data.List.Split ( splitOn )

data Dir = U | D | L | R
    deriving (Show, Eq)

-- (Grid, [Dir])
parse :: [String] -> [String]
parse str = let grid = splitOn [""] str !! 0
                dirs = map dir $ concat $ splitOn [""] str !! 1
            in grid

dir :: Char -> Dir
dir = \case '^' -> U; '>' -> R; '<' -> L; 'v' -> D

main :: IO()
main = do input <- lines <$> readFile "mini_input.txt"
          print $ parse input
