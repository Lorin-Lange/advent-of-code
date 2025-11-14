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
  go fix dsgn = sum [fix (drop (length t) dsgn) 
                    | t <- ts, t `isPrefixOf` dsgn]

main :: IO()
main = do input <- lines <$> readFile "input.txt"
          let (ps, dd) = splitOn ", " . head &&& drop 2 $ input
          putStrLn $ "Part 1: " ++ show (length $ filter ((>0) . ways ps) dd)
          putStrLn $ "Part 2: " ++ show (sum $ map (ways ps) dd)
