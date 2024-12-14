----------------------------------------------------
--              Advent of Code 2024               --
--            Day 14: Restroom Redoubt            --
--            Solution by Lorin Lange             --
----------------------------------------------------

{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Main where

import Data.List.Split ( splitOn )
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe )
import Data.Ord ( comparing )
import Data.List ( sortBy, sortOn )

data Robot = Robot
    { px, py :: Int
    , vx, vy :: Int }
    deriving Show

parse :: String -> [Robot]
parse = map parse' . lines where
    parse' s = let [p, v]   = map (drop 2) $ splitOn " " s
                   [px, py] = map read $ splitOn "," p
                   [vx, vy] = map read $ splitOn "," v
               in Robot { px, py, vx, vy }

move :: Int -> [Robot] -> [Robot]
move t = map move' where
    move' r = let px = (r.px + t * r.vx) `mod` 101
                  py = (r.py + t * r.vy) `mod` 103
              in r { px, py }

safetyFactor :: [Robot] -> Int
safetyFactor rbts = product [q1, q2, q3, q4]
    where q1 = sum [1 | r <- rbts, r.px < 50, r.py < 51]
          q2 = sum [1 | r <- rbts, r.px > 50, r.py < 51]
          q3 = sum [1 | r <- rbts, r.px < 50, r.py > 51]
          q4 = sum [1 | r <- rbts, r.px > 50, r.py > 51]

makeMap :: [Robot] -> Map.Map (Int, Int) Char
makeMap = Map.fromList . map (\r -> ((r.px, r.py), 'X'))

variance :: [Int] -> Double
variance lst = sum (map (\x -> (x - m)^2) l) / (n - 1)
    where l = map fromIntegral lst
          n = fromIntegral $ length l
          m = sum l / n

getMinVariance :: [Robot] -> (Robot -> Int) -> Int
getMinVariance rbts s = fst . head . sortOn snd $ 
    map (\t -> (t, variance . map s $ move t rbts)) [1..103]

getFewestNumber :: [Robot] -> Int
getFewestNumber rbts = bx + ((51 * (by-bx)) `mod` 103) * 101
    where bx = getMinVariance rbts $ \r -> r.px
          by = getMinVariance rbts $ \r -> r.py

printRobots :: [Robot] -> String
printRobots lst = concatMap (\y -> map (\x -> lu (x, y)) [0..100] ++ "\n") [0..102]
    where lu k = fromMaybe ' ' . Map.lookup k $ makeMap lst

getChristmasTree :: [Robot] -> Int -> IO()
getChristmasTree rbts n = writeFile "christmas_tree.txt" $ printRobots tree
    where tree = move n rbts

main :: IO()
main = do robots <- parse <$> readFile "input.txt"

          putStrLn $ "Part 1: " ++ show (safetyFactor $ move 100 robots)
          putStrLn $ "Part 2: " ++ show (getFewestNumber robots)
          getChristmasTree robots (getFewestNumber robots)
