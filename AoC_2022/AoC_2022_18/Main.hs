----------------------------------------------------
--              Advent of Code 2022               --
--            Day 18: Boiling Boulders            --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.List.Split ( splitOn )
import qualified Data.Set as Set
import Data.Foldable ( foldl' )
import Data.Ix ( inRange )

type Cube = (Int, Int, Int)

adjacent :: [Cube] -> Int
adjacent []     = 0
adjacent (x:xs) = length (filter (isAdjacent x) xs) + adjacent xs

neighbors :: Cube -> [Cube]
neighbors (x, y, z) = [(x+1, y, z), (x-1, y, z), (x, y+1, z), (x, y-1, z), (x, y, z+1), (x, y, z-1)]

isAdjacent :: Cube -> Cube -> Bool
isAdjacent c1 c2 = c1 `elem` neighbors c2

surface :: [Cube] -> Int
surface lst = length lst * 6 - adjacent lst * 2

trappedSurface :: [Cube] -> Int
trappedSurface lst = surface $ Set.toList $ allPoints Set.\\ (droplet `Set.union` outerPoints)
    where allPoints = makeSetOfAllPoints lst
          droplet   = Set.fromList lst
          (minX, minY, minZ, maxX, maxY, maxZ) = getExtrema lst
          outerPoints = flood droplet Set.empty (minX-1, minY-1, minZ-1)
          isInRange (x,y,z) = inRange (minX-1, maxX+1) x && inRange (minY-1, maxY+1) y && inRange (minZ-1, maxZ+1) z
          flood cs visited c = foldl' (flood cs) (c `Set.insert` visited) nexts
            where nexts = filter isOuter $ neighbors c
                  isOuter c = isInRange c && c `Set.notMember` visited && c `Set.notMember` cs

makeSetOfAllPoints :: [Cube] -> Set.Set Cube
makeSetOfAllPoints lst = Set.fromList 
    [(x,y,z) | x <- [minX-1..maxX+1], y <- [minY-1..maxY+1], z <- [minZ-1..maxZ+1]]
        where (minX, minY, minZ, maxX, maxY, maxZ) = getExtrema lst

getExtrema :: [Cube] -> (Int, Int, Int, Int, Int, Int)
getExtrema lst = (minX, minY, minZ, maxX, maxY, maxZ)
    where minX = minimum $ map getX lst
          minY = minimum $ map getY lst
          minZ = minimum $ map getZ lst
          maxX = maximum $ map getX lst
          maxY = maximum $ map getY lst
          maxZ = maximum $ map getZ lst
          getX (x, _, _) = x
          getY (_, y, _) = y
          getZ (_, _, z) = z

parse :: String -> Cube
parse str = (read $ head lst, read $ lst !! 1, read $ lst !! 2)
    where lst = splitOn "," str

main :: IO ()
main = do inp <- map parse . lines <$> readFile "input.txt"

          let totalSurface = surface inp
          putStrLn $ "Part 1: " ++ show totalSurface
          
          putStrLn $ "Part 2: " ++ show (totalSurface - trappedSurface inp)
