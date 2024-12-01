----------------------------------------------------
--              Advent of Code 2024               --
--           Day 1: Historian Hysteria            --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.List ( sort, group )
import Data.Maybe ( fromMaybe )
import Control.Monad  ( join )
import Control.Arrow ( (***) )

makeScore :: [(Int, Int)] -> Int -> Int
makeScore scores n = fromMaybe 0 (lookup n scores) * n

parse :: String -> ([Int], [Int])
parse = join (***) sort . unzip . map tuple . lines
    where tuple = (\[n1, n2] -> (n1, n2)) . map read . words

main :: IO()
main = do (lst1, lst2) <- parse <$> readFile "input.txt"

          putStrLn $ "Part 1: " ++ show (sum $ zipWith (\a b -> abs $ a - b) lst1 lst2)

          let freq = map (\l -> (head l, length l)) $ group lst2
          putStrLn $ "Part 2: " ++ show (sum $ map (makeScore freq) lst1)
