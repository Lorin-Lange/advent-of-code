----------------------------------------------------
--              Advent of Code 2024               --
--             Day 15: Warehouse Woes             --
--            Solution by Lorin Lange             --
----------------------------------------------------

{-# Language LambdaCase #-}

module Main where

import qualified Data.Map as Map
import Data.List.Split ( splitOn )

type Pos = (Int, Int)

type Grid = Map.Map Pos Item

data Item = Robot | Box | Wall | Free
    deriving (Show, Eq)

data Dir = U | D | L | R
    deriving (Show, Eq)

parse :: [String] -> (Grid, [Dir])
parse str = (grid, dirs)
    where grid = makeGrid . head $ splitOn [""] str
          dirs = map dir . concat $ splitOn [""] str !! 1
          dir = \case '^' -> U; '>' -> R; '<' -> L; 'v' -> D
          item = \case '#' -> Wall; '.' -> Free; '@' -> Robot; 'O' -> Box
          makeGrid = Map.fromList . concatMap 
                (\(y, l) -> zipWith (\ch x -> ((x, y), item ch)) l [0..]) . zip [0..]

next :: Pos -> Dir -> Pos
next (x, y) U = (x, y-1)
next (x, y) D = (x, y+1)
next (x, y) L = (x-1, y)
next (x, y) R = (x+1, y)

move :: (Pos, Grid) -> [Dir] -> (Pos, Grid)
move = foldl move'

move' :: (Pos, Grid) -> Dir -> (Pos, Grid)
move' t@(p, g) d | not $ isPossible t d = t
                 | otherwise            = (next p d, shift t d Robot)

shift :: (Pos, Grid) -> Dir -> Item -> Grid
shift (p, g) d i = case Map.lookup p g of
    (Just Robot) -> shift (next p d, Map.insert p Free g) d Robot
    (Just Box)   -> shift (next p d, Map.insert p i g) d Box
    (Just Wall)  -> g
    (Just Free)  -> Map.insert p i g

isPossible :: (Pos, Grid) -> Dir -> Bool
isPossible (p, g) d = case Map.lookup p g of
    (Just Wall) -> False
    (Just Free) -> True
    (Just _)    -> isPossible (next p d, g) d
    Nothing     -> False

getPos :: Grid -> Pos
getPos g = head [p | (p, Robot) <- Map.toList g]

sumOfBoxes :: Grid -> Int
sumOfBoxes g = sum [100 * y + x | ((x,y), Box) <- Map.toList g]

main :: IO()
main = do t@(grid, dirs) <- parse . lines <$> readFile "input.txt"
          let s = getPos grid
          print s
          let (_, g) = move (s, grid) dirs
          print $ sumOfBoxes g
