----------------------------------------------------
--              Advent of Code 2024               --
--            Day 14: Restroom Redoubt            --
--            Solution by Lorin Lange             --
----------------------------------------------------

{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Main where

import Data.List.Split ( splitOn )

data Robot = Robot
    { px, py :: Int
    , vx, vy :: Int }
    deriving Show

parse :: String -> [Robot]
parse = map parse' . lines where 
    parse' s = let [p, v]   = map (drop 2) $ splitOn " " s
                   [px, py] = map read $ splitOn "," p
                   [vx, vy] = map read $ splitOn "," v
               in Robot { px, py, vx, vy }

move :: [Robot] -> [Robot]
move = map (move' 100) where 
    move' t r = let px = (r.px + t * r.vx) `mod` 101
                    py = (r.py + t * r.vy) `mod` 103
                in r { px, py }

safetyFactor :: [Robot] -> Int
safetyFactor rbts = q1 * q2 * q3 * q4
    where q1 = sum [1 | r <- rbts, r.px < 50, r.py < 51]
          q2 = sum [1 | r <- rbts, r.px > 50, r.py < 51]
          q3 = sum [1 | r <- rbts, r.px < 50, r.py > 51]
          q4 = sum [1 | r <- rbts, r.px > 50, r.py > 51]

main :: IO()
main = do robots <- parse <$> readFile "input.txt"

          putStrLn $ "Part 1: " ++ show (safetyFactor $ move robots)
          putStrLn $ "Part 1: " ++ show ("TODO")
