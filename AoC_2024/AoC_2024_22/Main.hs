----------------------------------------------------
--              Advent of Code 2024               --
--             Day 22: Monkey Market              --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.Composition ((.:))
import Data.Bits ((.|.), shiftL, shiftR, xor)
import qualified Data.IntMap as IntMap

next :: Int -> Int
next = fun (* 2048) . fun (`div` 32) . fun (* 64)
    where fun f i   = mixNPrune i $ f i
          mixNPrune = (`mod` 16_777_216) .: xor

secrets :: Int -> [Int]
secrets = take 2001 . iterate next

absurdNumberOfBananas :: [Int] -> Int
absurdNumberOfBananas lst = maximum $ IntMap.elems m
    where ps = map (map (`mod` 10) . secrets) lst
          m  = IntMap.unionsWith (+) $ map bananas ps

bananas :: [Int] -> IntMap.IntMap Int
bananas l = IntMap.fromListWith (\_ x -> x) lst
    where seq = scanl (\acc x -> ((x + 9) `shiftL` 15) .|. (acc `shiftR` 5)) 0
          lst = drop 4 $ zip (seq $ changes l) l
          changes ps = zipWith (-) (tail ps) ps

main :: IO()
main = do lst <- map read . lines <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (sum $ map (last . secrets) lst) 
          putStrLn $ "Part 2: " ++ show (absurdNumberOfBananas lst)
