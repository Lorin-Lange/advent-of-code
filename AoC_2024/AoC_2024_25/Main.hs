----------------------------------------------------
--              Advent of Code 2024               --
--             Day 25: Code Chronicle             --
--            Solution by Lorin Lange             --
----------------------------------------------------

{-# Language LambdaCase #-}

module Main where

import Data.List.Split (splitOn)
import Data.List (partition, transpose)
import Control.Monad (join, liftM2)
import Control.Arrow ((***))
import Data.Bifunctor (second)

parse :: [String] -> ([[Int]], [[Int]])
parse lst = join (***) (map $ map c . transpose) $ second (map reverse) t
    where t = partition (all (== '#') . head) $ splitOn [""] lst
          c = \case [] -> -1; ('.':_) -> -1; ('#':t) -> 1 + c t

main :: IO()
main = do cs <- uncurry (liftM2 (,)) . parse . lines <$> readFile "input.txt"
          let fits = all (<= 5) . uncurry (zipWith (+))
          putStrLn $ "Part 1: " ++ show (length $ filter fits cs)
