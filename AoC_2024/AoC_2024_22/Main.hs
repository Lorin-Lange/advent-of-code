----------------------------------------------------
--              Advent of Code 2024               --
--           🐵Day 22: Monkey Market🐒            --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.Composition ( (.:) )
import Data.Bits ( (.|.), shiftL, shiftR, xor )
import qualified Data.IntMap as IntMap

next :: Int -> Int
next = fun (* 2048) . fun (`div` 32) . fun (* 64)
    where mixNPrune = (`mod` 16_777_216) .: xor
          fun f i = mixNPrune i $ f i

secrets :: Int -> [Int]
secrets = take 2001 . iterate next

priceChanges :: Num c => [c] -> [c]
priceChanges ps = zipWith (-) (tail ps) ps

absurdNumberOfBananas :: [Int] -> Int
absurdNumberOfBananas lst = maximum $ IntMap.elems m
    where m = IntMap.unionsWith (+) $ map bananas prices
          prices = map (map (`mod` 10) . secrets) lst

bananas :: [Int] -> IntMap.IntMap Int
bananas l = IntMap.fromListWith (\_ x -> x) lst
    where seq = scanl (\acc x -> ((x + 9) `shiftL` 15) .|. (acc `shiftR` 5)) 0
          lst = drop 4 $ zip (seq $ priceChanges l) l

main :: IO()
main = do lst <- map read . lines <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (sum $ map (last . secrets) lst) 
          putStrLn $ "Part 2: " ++ show (absurdNumberOfBananas lst) 