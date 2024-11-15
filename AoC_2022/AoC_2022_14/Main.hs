----------------------------------------------------
--              Advent of Code 2022               --
--           Day 14: Regolith Reservoir           --
--            Solution by Lorin Lange             --
----------------------------------------------------

{-# LANGUAGE TupleSections #-}

module Main where

import Data.List.Split ( endBy, splitOn )
import qualified Data.Map as Map

data Material = Rock | Sand | Floor
    deriving (Show, Eq)

type Grid = Map.Map (Int, Int) Material

getPoints :: [(Int, Int)] -> [(Int, Int)]
getPoints []         = []
getPoints [_]        = []
getPoints ((c1,r1):(c2,r2):xs)
    | c1 == c2 && r1 < r2 = map (c1,) [r1..r2] ++ getPoints ((c2,r2):xs)
    | c1 == c2 && r1 > r2 = map (c1,) [r2..r1] ++ getPoints ((c2,r2):xs)
    | r1 == r2 && c1 < c2 = map (,r1) [c1..c2] ++ getPoints ((c2,r2):xs)
    | r1 == r2 && c1 > c2 = map (,r1) [c2..c1] ++ getPoints ((c2,r2):xs)
    | otherwise = error "Error while expanding points"
    -- = case!?

check :: Int -> (Int, Int) -> Grid -> Int -> Int
check max k@(c, r) g counter
    | r > max = counter
    | otherwise =
    case Map.lookup (c, r+1) g of
        (Just _) -> case Map.lookup (c-1, r+1) g of
                        (Just _) -> case Map.lookup (c+1, r+1) g of
                                        (Just _) -> check max (500,-1) (Map.insert k Sand g) (counter + 1)
                                        Nothing    -> check max (c+1, r+1) g counter
                        Nothing  -> check max (c-1, r+1) g counter
        Nothing  -> check max (c, r+1) g counter

check2 :: (Int, Int) -> Grid -> Int -> Int
check2 k@(c, r) g counter =
    case Map.lookup (500, 0) g of
        (Just _) -> counter
        Nothing  ->
                    case Map.lookup (c, r+1) g of
                        (Just _) -> case Map.lookup (c-1, r+1) g of
                                        (Just _) -> case Map.lookup (c+1, r+1) g of
                                                        (Just _) -> check2 (500,-1) (Map.insert k Sand g) (counter + 1)
                                                        Nothing    -> check2 (c+1, r+1) g counter
                                        Nothing  -> check2 (c-1, r+1) g counter
                        Nothing  -> check2 (c, r+1) g counter





parse :: String -> [(Int, Int)]
parse = map (\st -> (read $ head $ splitOn "," st, read $ splitOn "," st !! 1)) . endBy " -> "

main :: IO()
main = do
    inp <- map parse . lines <$> readFile "input.txt"
    let lst = map (, Rock) $ concatMap getPoints inp
    let maxR = maximum $ map (snd . fst) lst
    let m = Map.fromList lst
    print $ check maxR (500, -1) m 0

    let minC = minimum $ map (fst . fst) lst
    let maxC = maximum $ map (fst . fst) lst
    let floor = map (, Floor) (getPoints [(minC - minC, maxR+2), (maxC+maxC, maxR+2)])
    let m' = Map.fromList $ lst ++ floor
    print $ check2 (500, -1) m' 0
