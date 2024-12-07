----------------------------------------------------
--              Advent of Code 2024               --
--             Day 6: Guard Gallivant             --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (isNothing)

type Pos = (Int, Int)

type Grid = Map.Map Pos Char

move :: (Pos, Grid) -> (Pos, Grid)
move (p, g) = case Map.lookup p g of
    Nothing   -> (p, g)
    (Just ch) -> move $ lookupGrid g p ch

lookupGrid :: Grid -> Pos -> Char -> (Pos, Grid)
lookupGrid g p@(r, c) '^' | isFree g (r-1, c) = ((r-1, c), updateGrid '^' p (r-1, c) g)
                          | isNothing (Map.lookup (r-1, c) g) = ((r-1, c), Map.insert p 'X' g)
                          | otherwise         = (p, Map.insert p '>' g)
lookupGrid g p@(r, c) 'v' | isFree g (r+1, c) = ((r+1, c), updateGrid 'v' p (r+1, c) g)
                          | isNothing (Map.lookup (r+1, c) g) = ((r+1, c), Map.insert p 'X' g)
                          | otherwise         = (p, Map.insert p '<' g)
lookupGrid g p@(r, c) '>' | isFree g (r, c+1) = ((r, c+1), updateGrid '>' p (r, c+1) g)
                          | isNothing (Map.lookup (r, c+1) g) = ((r, c+1), Map.insert p 'X' g)
                          | otherwise         = (p, Map.insert p 'v' g)
lookupGrid g p@(r, c) '<' | isFree g (r, c-1) = ((r, c-1), updateGrid '<' p (r, c-1) g)
                          | isNothing (Map.lookup (r, c-1) g) = ((r, c-1), Map.insert p 'X' g)
                          | otherwise         = (p, Map.insert p '^' g)
lookupGrid _ _ _   = error "unknown char"

updateGrid :: Char -> Pos -> Pos -> Grid -> Grid
updateGrid ch oldPos newPos g = Map.insert newPos ch (Map.insert oldPos 'X' g)

isFree :: Grid -> Pos -> Bool
isFree g p = case Map.lookup p g of
    (Just '.') -> True
    (Just 'X') -> True
    _  -> False

makeGrid :: [String] -> Grid
makeGrid = Map.fromList . concatMap 
    (\(r, l) -> zipWith (\c ch -> ((r, c), ch)) [0..] l) . zip [0..]

getStart :: Grid -> Pos
getStart = fst . head . filter (\(pos, ch) -> ch /= '.' && ch /= '#') . Map.toList

main :: IO()
main = do grid <- makeGrid . lines <$> readFile "input.txt"
          let (_, g') = move (getStart grid, grid)
          print $ length $ filter (\(_, ch) -> ch == 'X') $ Map.toList g'
