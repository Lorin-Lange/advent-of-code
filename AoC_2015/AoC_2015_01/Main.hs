----------------------------------------------------
--              Advent of Code 2015               --
--             Day 1: Not Quite Lisp              --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.List ( findIndex )
import Data.Maybe ( fromJust )

main :: IO()
main = do let move n '(' = n+1; move n ')' = n-1
          lst <- scanl move 0 <$> readFile "input.txt"

          putStrLn $ "Part 1: " ++ show (last lst)
          putStrLn $ "Part 2: " ++ show (fromJust $ findIndex (< 0) lst)
