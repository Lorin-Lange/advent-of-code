----------------------------------------------------
--              Advent of Code 2024               --
--              Day 19: Linen Layout              --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.MemoTrie (memoFix)
import Control.Arrow ((&&&))

ways :: [String] -> String -> Int
ways ts = memoFix go where
  go _   ""   = 1
  go fix dsgn = sum [fix $ drop (length t) dsgn | t <- ts, t `isPrefixOf` dsgn]

main :: IO()
main = do input <- lines <$> readFile "input.txt"
          let (ps, dd) = splitOn ", " . head &&& drop 2 $ input
          putStrLn $ "Part 1: " ++ show (sum [1 | d <- dd, ways ps d > 0])
          putStrLn $ "Part 2: " ++ show (sum [ways ps d | d <- dd])
