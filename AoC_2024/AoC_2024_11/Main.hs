----------------------------------------------------
--              Advent of Code 2024               --
--           Day 11: Plutonian Pebbles            --
--            Solution by Lorin Lange             --
----------------------------------------------------

{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Map as Map

type StoneMap = Map.Map Int Int -- Number and count of it.

add :: Int -> Int -> StoneMap -> StoneMap
add k v = Map.alter increaseCount k -- O(log n). 
    where increaseCount Nothing   = Just v
          increaseCount (Just v') = Just $ v + v'

blinking :: StoneMap -> Int -> Int -> StoneMap
blinking m 0 b = add 1 b m
blinking m i b | odd len   = add (i * 2024) b m
               | otherwise = (add i1 b . add i2 b) m
    where len = length $ show i
          (s1, s2) = splitAt (len `div` 2) (show i)
          (i1, i2) = (read s1, read s2)

blink :: Int -> StoneMap -> Int
blink n = sum . Map.elems . last . take (n + 1) . iterate update
    where update = Map.foldlWithKey blinking Map.empty

main :: IO()
main = do input <- map read . words <$> readFile "input.txt"
          let m = Map.fromList $ map (,1) input

          putStrLn $ "Part 1: " ++ show (blink 25 m)
          putStrLn $ "Part 2: " ++ show (blink 75 m)
