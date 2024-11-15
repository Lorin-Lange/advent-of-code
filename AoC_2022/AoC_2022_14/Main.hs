
{-# LANGUAGE TupleSections #-}

module Main where

import Data.List.Split ( endBy, splitOn )
import qualified Data.Map as Map

data Material = Rock | Sand
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
        (Just m) -> case Map.lookup (c-1, r+1) g of
                        (Just m') -> case Map.lookup (c+1, r+1) g of
                                        (Just m'') -> check max (500,-1) (Map.insert k Sand g) (counter + 1)
                                        Nothing    -> check max (c+1, r+1) g counter
                        Nothing   -> check max (c-1, r+1) g counter
        Nothing  -> check max (c, r+1) g counter

parse :: String -> [(Int, Int)]
parse = map (\st ->
    (read $ head $ splitOn "," st, read $ splitOn "," st !! 1))
    . endBy " -> "

main :: IO()
main = do
    inp <- map parse . lines <$> readFile "input.txt"
    let lst = map (, Rock) $ concatMap getPoints inp
    let max = maximum $ map (snd . fst) lst
    let m = Map.fromList lst
    print $ check max (500, -1) m 0