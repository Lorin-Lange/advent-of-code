----------------------------------------------------
--              Advent of Code 2022               --
--           Day 14: Regolith Reservoir           --
--            Solution by Lorin Lange             --
----------------------------------------------------

{-# LANGUAGE TupleSections #-}

module Main where

import Data.List.Split ( endBy, splitOn )
import qualified Data.Map as Map
import Data.Maybe ( maybe )

data Material = Rock | Sand | Floor
    deriving (Show, Eq)

type Grid = Map.Map (Int, Int) Material

type Check = (Int, Int) -> Grid -> Int -> Int

getPoints :: [(Int, Int)] -> [(Int, Int)]
getPoints []  = []
getPoints [_] = []
getPoints ((c1,r1):(c2,r2):xs)
    | c1 == c2 && r1 < r2 = map (c1,) [r1..r2] ++ getPoints ((c2,r2):xs)
    | c1 == c2 && r1 > r2 = map (c1,) [r2..r1] ++ getPoints ((c2,r2):xs)
    | r1 == r2 && c1 < c2 = map (,r1) [c1..c2] ++ getPoints ((c2,r2):xs)
    | r1 == r2 && c1 > c2 = map (,r1) [c2..c1] ++ getPoints ((c2,r2):xs)

check :: Check -> Check
check f k@(c, r) g counter = mbH (Map.lookup (c, r+1) g) (f (c, r+1) g counter) 
                           . mbH (Map.lookup (c-1, r+1) g) (f (c-1, r+1) g counter)
                           . mbH (Map.lookup (c+1, r+1) g) (f (500,-1) (Map.insert k Sand g) (counter + 1)) 
                           $ f (c+1, r+1) g counter
    where mbH (Just _) _ c = c; mbH Nothing  v _ = v

check1 :: Int -> Check
check1 max k@(_, r) g counter =
    if r > max then counter else check (check1 max) k g counter

check2 :: Check
check2 k g counter =
    case Map.lookup (500, 0) g of
        (Just _) -> counter
        Nothing  -> check check2 k g counter

parse :: String -> [(Int, Int)]
parse = map (\st -> (read $ head $ splitOn "," st, read $ splitOn "," st !! 1)) . endBy " -> "

main :: IO()
main = do inp <- map parse . lines <$> readFile "input.txt"
          let lst = map (, Rock) $ concatMap getPoints inp
          let maxR = maximum $ map (snd . fst) lst

          putStrLn $ "Part 1: " ++ show (check1 maxR (500, -1) (Map.fromList lst) 0)

          let minC = minimum $ map (fst . fst) lst
          let maxC = maximum $ map (fst . fst) lst
          let floor = map (, Floor) (getPoints [(minC - minC, maxR+2), (maxC + maxC, maxR+2)])
          putStrLn $ "Part 2: " ++ show (check2 (500, -1) (Map.fromList $ lst ++ floor) 0)
