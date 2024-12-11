----------------------------------------------------
--              Advent of Code 2024               --
--           Day 11: Plutonian Pebbles            --
--            Solution by Lorin Lange             --
----------------------------------------------------

{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Map as Map

-- Number (key) and count of it (value).
type StoneMap = Map.Map Int Int

add :: Int -> Int -> StoneMap -> StoneMap
add k v = Map.alter increaseCount k -- O(log n). 
    where increaseCount Nothing    = Just v
          increaseCount j@(Just _) = (+v) <$> j

blinking :: StoneMap -> Int -> Int -> StoneMap
blinking m 0 c = add 1 c m
blinking m i c | even len  = (add i1 c . add i2 c) m
               | otherwise = add (i * 2024) c m
    where len      = length $ show i
          (s1, s2) = splitAt (len `div` 2) $ show i
          (i1, i2) = (read s1, read s2)

blink :: Int -> StoneMap -> Int
blink n = sum . Map.elems . last . take (n + 1) . iterate update
    where update = Map.foldlWithKey blinking Map.empty

main :: IO()
main = do lst <- map read . words <$> readFile "input.txt"
          let m = Map.fromList $ map (,1) lst

          putStrLn $ "Part 1: " ++ show (blink 25 m)
          putStrLn $ "Part 2: " ++ show (blink 75 m)
