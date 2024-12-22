----------------------------------------------------
--              Advent of Code 2024               --
--             Day 22: Monkey Market              --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.Bits ( xor )

mix :: Integer -> Integer -> Integer
mix sn gv = sn `xor` gv

prune :: Integer -> Integer
prune n = n `mod` 16_777_216

next :: Integer -> Integer
next n = prune $ mix step2 (step2 * 2048)
    where step1 = prune $ mix n (64 * n)
          step2 = prune $ mix step1 (step1 `div` 32)

newSecretNumber :: Integer -> Integer
newSecretNumber = last . take 2001 . iterate next

main :: IO()
main = do lst <- map read . lines <$> readFile "input.txt"

          putStrLn $ "Part 1: " ++ show (sum $ map newSecretNumber lst)

          putStrLn $ "Part 2: " ++ show ("")
