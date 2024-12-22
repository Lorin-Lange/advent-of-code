----------------------------------------------------
--              Advent of Code 2024               --
--             Day 22: Monkey Market              --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.Bits ( xor )
import Data.Composition ( (.:) )

next :: Integer -> Integer
next = fun (* 2048) . fun (`div` 32) . fun (* 64)
    where mixAndPrune = (`mod` 16_777_216) .: xor
          fun f i = mixAndPrune i $ f i

secretNumber2000 :: Integer -> Integer
secretNumber2000 = last . take 2001 . iterate next

priceChanges :: Integer -> [Integer]
priceChanges n = zipWith (-) (tail prices) prices
    where secrets = take 2001 $ iterate next n
          prices  = map (`mod` 10) secrets

main :: IO()
main = do lst <- map read . lines <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (sum $ map secretNumber2000 lst)

          putStrLn $ "Part 2: " ++ show ("")
