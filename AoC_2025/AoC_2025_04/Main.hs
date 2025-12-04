----------------------------------------------------
--              Advent of Code 2025               --
--           Day 4: Printing Department           --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main (main) where

import Data.List.Extra ((!?))
import Control.Lens ((&), (.~), Ixed(ix))

type Pos = (Int, Int)

type Grid = [[Char]]

neighbors :: Grid -> Pos -> Int
neighbors g (x, y) = sum
   [ isRoll g (x + dx, y + dy)
   | dx <- [-1..1], dy <- [-1..1]
   , (dx, dy) /= (0, 0)]

getCell :: Grid -> Pos -> Maybe Char
getCell g (x, y) = g !? y >>= (!? x)

isRoll :: Grid -> Pos -> Int
isRoll g p = maybe 0 (fromEnum . (== '@')) $ getCell g p

accessible :: Grid -> Pos -> Bool
accessible g p = Just '@' == getCell g p && neighbors g p < 4

remove :: Grid -> Pos -> Grid
remove g (x, y) = g & ix y . ix x .~ '.'

step :: Grid -> (Grid, Int)
step g = (foldl remove g ps, length ps)
   where ps = [(x, y) | y <- r, x <- r, accessible g (x, y)]
         r  = [0..length g - 1]

simulate :: Grid -> Int
simulate g = go g 0
   where go grid total =
          case step grid of
            (_, 0)     -> total
            (grid', n) -> go grid' $ total + n

main :: IO()
main = do grid <- lines <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (snd $ step grid)
          putStrLn $ "Part 2: " ++ show (simulate grid)
