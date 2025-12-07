----------------------------------------------------
--              Advent of Code 2025               --
--              Day 7: Laboratories               --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Pos = (Int, Int)

type Grid = Map.Map Pos Char

type PM = Map.Map Pos Integer

parse :: [String] -> (Grid, Pos, Int)
parse rows = (Map.fromList ps, s, maxY)
   where maxY = maximum $ map (snd . fst) ps
         ps   = [ ((x, y), c) 
                | (y, row) <- zip [0..] rows
                , (x, c)   <- zip [0..] row ]
         s    = fst . head $ filter (('S' ==) . snd) ps

reachables :: Grid -> Pos -> [Pos]
reachables g s = Set.toList $ go Set.empty [s]
   where go seen [] = seen
         go seen (curr:rest) 
            | curr `Set.member` seen = go seen rest
            | otherwise              = go (Set.insert curr seen) 
                                          (nexts g curr ++ rest)

nexts :: Grid -> Pos -> [Pos]
nexts g p@(x, y) = case Map.lookup p g of
    Just '.' -> [(x, y + 1)] 
    Just 'S' -> [(x, y + 1)] 
    Just '^' -> [(x - 1, y), (x + 1, y)]
    _        -> []

next :: Int -> Grid -> (PM, Integer) -> Pos -> Integer -> (PM, Integer)
next maxY g (accMap, accExit) (x, y) count
    | y > maxY  = (accMap, accExit + count)
    | otherwise = case Map.lookup (x, y) g of
        Just '^'  -> (addSplit accMap count,       accExit)
        _         -> (add (x, y + 1) count accMap, accExit)
    where addSplit mp c = add (x + 1, y) c $ add (x - 1, y) c mp
          add           = Map.insertWith (+)

part1 :: (Grid, Pos, Int) -> Int
part1 (g, s, _) = length 
                . filter ((Just '^' ==) . flip Map.lookup g) 
                $ reachables g s

part2 :: (Grid, Pos, Int) -> Integer
part2 (g, s, maxY) = go (Map.singleton s 1) 0
  where go ptcs completed | Map.null ptcs = completed
                          | otherwise     = go n (completed + exit)
           where (n, exit) = Map.foldlWithKey (next maxY g) (Map.empty, 0) ptcs

main :: IO()
main = do input <- parse . lines <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (part1 input)
          putStrLn $ "Part 2: " ++ show (part2 input)
