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
import Data.List ( sortBy )

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
safetyFactor rbts = q1 * q2 * q3 * q4
    where q1 = sum [1 | r <- rbts, r.px < 50, r.py < 51]
          q2 = sum [1 | r <- rbts, r.px > 50, r.py < 51]
          q3 = sum [1 | r <- rbts, r.px < 50, r.py > 51]
          q4 = sum [1 | r <- rbts, r.px > 50, r.py > 51]

printRobots :: [Robot] -> String
printRobots lst = concatMap (\y -> map (\x -> lu (x, y)) [0..101] ++ "\n") [0..103]
    where m = makeMap lst
          lu k = fromMaybe ' ' $ Map.lookup k m

makeMap :: [Robot] -> Map.Map (Int, Int) Char
makeMap lst = Map.fromList $ map (\r -> ((r.px, r.py), 'X')) lst

computeRobots :: [Robot] -> [((Int, [Robot]), Int)]
computeRobots rbts = sortBy (comparing snd) robots
    where robots = map (\n -> ((n, move n rbts), safetyFactor $ move n rbts)) [0..101 * 103]

main :: IO()
main = do robots <- parse <$> readFile "input.txt"

          putStrLn $ "Part 1: " ++ show (safetyFactor $ move 100 robots)

          let robots' = computeRobots robots
          let robot = snd $ fst $ head robots'
          writeFile "tree.txt" $ printRobots robot
          putStrLn $ "Part 2: " ++ show (fst $ fst $ head robots')
