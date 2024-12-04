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
    where n  = isMAS (r-1,c)   (r-2,c)   (r-3,c)
          e  = isMAS (r,  c+1) (r,  c+2) (r,  c+3)
          s  = isMAS (r+1,c)   (r+2,c)   (r+3,c)
          w  = isMAS (r,  c-1) (r,  c-2)   (r,c-3)
          ne = isMAS (r-1,c+1) (r-2,c+2) (r-3,c+3)
          se = isMAS (r+1,c+1) (r+2,c+2) (r+3,c+3)
          sw = isMAS (r+1,c-1) (r+2,c-2) (r+3,c-3)
          nw = isMAS (r-1,c-1) (r-2,c-2) (r-3,c-3)
          isMAS p1 p2 p3 = isChar m 'M' p1 && isChar m 'A' p2 && isChar m 'S' p3

mas :: Map.Map Pos (Char, Pos) -> Map.Map Pos Int
mas m = fmap (isMAS m) m

isMAS :: Map.Map Pos (Char, Pos) -> (Char, Pos) -> Int
isMAS m (ch, (r,c)) | ch /= 'A' = 0
                    | otherwise = length $ filter id [c1,c2,c3,c4]
    where c1 = isX 'S' 'S' 'M' 'M'
          c2 = isX 'M' 'S' 'S' 'M'
          c3 = isX 'S' 'M' 'M' 'S'
          c4 = isX 'M' 'M' 'S' 'S'
          isX l1 l2 l3 l4 = isChar m l1 (r-1,c+1) && isChar m l2 (r+1,c+1) && 
                            isChar m l3 (r+1,c-1) && isChar m l4 (r-1,c-1)

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

    putStrLn $ "Part 2: " ++ show (sum $ Map.elems $ mas m)
