----------------------------------------------------
--              Advent of Code 2025               --
--                  Day 3: Lobby                  --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main (main) where

import Data.Char (digitToInt)
import Data.List (foldl')

maxBank :: String -> Int
maxBank = snd . foldl' step (-1, 0) . map digitToInt
  where step (-1, res) d = (d, res)
        step (m,  res) d = (max m d, max res $ m * 10 + d)

best12 :: String -> Integer
best12 = read . go 12
  where go 0 _  = []
        go n xs = c : go (n - 1) rest
          where c           = maximum $ take (length xs - n + 1) xs
                (_, _:rest) = break (== c) xs

main :: IO()
main = do input <- lines <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (sum $ map maxBank input)
          putStrLn $ "Part 2: " ++ show (sum $ map best12 input)
