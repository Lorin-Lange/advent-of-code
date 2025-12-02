----------------------------------------------------
--              Advent of Code 2025               --
--                Day 2: Gift Shop                --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main (main) where

import Data.List.Split (splitOn)

parse :: String -> [Integer]
parse = concatMap makeRange . splitOn ","
  where makeTuple s = (read $ head s, read $ s !! 1)
        makeRange l = let (a, b) = makeTuple $ splitOn "-" l in [a..b]

isInvalid1 :: Integer -> Bool
isInvalid1 n = even len && a == b
  where (a, b) = splitAt (len `div` 2) $ show n
        len    = length $ show n

isInvalid2 :: Integer -> Bool
isInvalid2 n = any inValid [1 .. len `div` 2]
  where s         = show n
        len       = length s
        inValid k = len `mod` k == 0 && 
            concat (replicate (len `div` k) $ take k s) == s

part1 :: [Integer] -> Integer
part1 = sum . filter isInvalid1

part2 :: [Integer] -> Integer
part2 = sum . filter isInvalid2

main :: IO()
main = do input <- parse <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (part1 input)
          putStrLn $ "Part 2: " ++ show (part2 input)
