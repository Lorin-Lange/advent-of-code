----------------------------------------------------
--              Advent of Code 2024               --
--               Day 23: LAN Party                --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main(main) where

import Data.List.Split (splitOn)
import Data.Bifunctor (second)
import Data.List (sort, maximumBy, intercalate)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Graph = Map.Map String (Set String)

neighbors :: Graph -> String -> Set String
neighbors g v = Map.findWithDefault Set.empty v g

intoLT :: [(String, String)] -> Graph
intoLT = Map.fromListWith Set.union . concatMap
    (\(a,b) -> [(a, Set.singleton b), (b, Set.singleton a)])

intersection :: (String, String) -> Graph -> Set String
intersection (a, b) g = neighbors g a `Set.intersection` neighbors g b

makePairs :: [String] -> [(String, String)]
makePairs = map split 
    where split s = let [a, b] = splitOn "-" s in (a, b)

makeTriads :: [(String, String)] -> Graph -> [[String]]
makeTriads ps g = Set.toList . Set.fromList $ concatMap f ps
    where f p@(a, b) = map (\c -> sort [a, b, c]) 
                     . Set.toList $ intersection p g

filterForT :: [[String]] -> [[String]]
filterForT = filter . any $ \s -> head s == 't'

bronKerbosch :: Graph -> [Set String]
bronKerbosch g = go Set.empty (Map.keysSet g) Set.empty
    where go r p x | Set.null p && Set.null x = [r]
                   | otherwise                = loop p
            where loop q      = maybe [] f (Set.minView q)
                  f (v, rest) = go (Set.insert v r) 
                        (rest `Set.intersection` neighbors g v) 
                        (x `Set.intersection` neighbors g v) ++ loop rest

maxClique :: Graph -> Set String
maxClique = maximumBy (comparing Set.size) . bronKerbosch

main :: IO()
main = do pairs <- makePairs . lines <$> readFile "input.txt"
          let res1 = length . filterForT . makeTriads pairs $ intoLT pairs
          putStrLn $ "Part 1: " ++ show res1
          let res2 = intercalate "," . sort . Set.toList . maxClique $ intoLT pairs
          putStrLn $ "Part 2: " ++ show res2
