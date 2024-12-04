----------------------------------------------------
--              Advent of Code 2024               --
--              Day 4: Ceres Search               --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import qualified Data.Map as Map

type Pos = (Int, Int)

xmas :: Map.Map Pos (Char, Pos) -> Map.Map Pos Int
xmas m = fmap (isXMAS m) m

isXMAS :: Map.Map Pos (Char, Pos) -> (Char, Pos) -> Int
isXMAS m (ch, (r,c)) | ch /= 'X' = 0
                     | otherwise = length $ filter id [n, e, s, w, ne, se, sw, nw]
    where n  = isMAS m (r-1,c) (r-2,c) (r-3,c)
          e  = isMAS m (r,c+1) (r,c+2) (r,c+3)
          s  = isMAS m (r+1,c) (r+2,c) (r+3,c)
          w  = isMAS m (r,c-1) (r,c-2) (r,c-3)
          ne = isMAS m (r-1,c+1) (r-2,c+2) (r-3,c+3)
          se = isMAS m (r+1,c+1) (r+2,c+2) (r+3,c+3)
          sw = isMAS m (r+1,c-1) (r+2,c-2) (r+3,c-3)
          nw = isMAS m (r-1,c-1) (r-2,c-2) (r-3,c-3)

isMAS :: Map.Map Pos (Char, Pos) -> Pos -> Pos -> Pos -> Bool
isMAS m p1 p2 p3 = isChar m 'M' p1 && isChar m 'A' p2 && isChar m 'S' p3

isChar :: Map.Map Pos (Char, Pos) -> Char -> Pos -> Bool
isChar m c p = case Map.lookup p m of
    Nothing        -> False
    (Just (ch, _)) -> ch == c

main :: IO()
main = do
    input <- lines <$> readFile "input.txt"
    let lst = concat $ zipWith (\r l -> zipWith (\c ch -> ((r, c), (ch, (r, c)))) [0..] l) [0..] input
    let m = Map.fromList lst
    putStrLn $ "Part 1: " ++ show (sum $ Map.elems $ xmas m)
