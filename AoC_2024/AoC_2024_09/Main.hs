----------------------------------------------------
--              Advent of Code 2024               --
--             Day 9: Disk Fragmenter             --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.Char ( digitToInt, isAlphaNum )
import Data.List.Split ( chunksOf )
import Data.Maybe ( catMaybes, fromMaybe )

-- The first Int in File denotes the size.
data Space = File Int Int | Free Int
    deriving Show

expand :: [Int] -> [Maybe Int]
expand = concatMap f . zip [0..] . chunksOf 2
    where f (idx, [c])      = replicate c $ Just idx
          f (idx, [c1, c2]) = replicate c1 (Just idx) 
                ++ replicate c2 Nothing

checkSum :: [Int] -> Integer
checkSum = sum . map toInteger . zipWith (*) [0..]

combine :: [Maybe Int] -> [Int]
combine lst = combineH lst (reverse lst) where
    combineH []             _  = []
    combineH ((Just i): xs) ys = i : combineH xs ys
    combineH (Nothing: xs)  ys = i : combineH xs ys' 
        where (i, ys') = get ys
              get ((Just j): xs') = (j, xs')
              get (Nothing: xs')  = get xs'

getLength :: [Maybe Int] -> Int
getLength = length . catMaybes

main :: IO()
main = do input <- map digitToInt <$> readFile "input.txt"
          let len = getLength $ expand input
          putStrLn $ "Part 1: " ++ show (checkSum . take len . combine $ expand input)
