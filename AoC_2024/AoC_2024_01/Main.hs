----------------------------------------------------
--              Advent of Code 2024               --
--           Day 1: Historian Hysteria            --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.List ( sort, group )
import Data.Maybe ( fromMaybe )
import Data.Composition ( (.:) )
import Control.Monad  ( join )
import Control.Arrow ( (***) )
import qualified Data.IntMap.Strict as IntMap

makeScore :: IntMap.IntMap Int -> Int -> Int
makeScore m n = IntMap.findWithDefault 0 n m * n

parse :: String -> ([Int], [Int])
parse = join (***) sort . unzip . map toTuple . lines
    where toTuple = (\[n1, n2] -> (n1, n2)) . map read . words

main :: IO()
main = do (lst1, lst2) <- parse <$> readFile "input.txt"

          putStrLn $ "Part 1: " ++ show (sum $ zipWith (abs .: (-)) lst1 lst2)

          let freq = IntMap.fromList . map (\l -> (head l, length l)) $ group lst2
          putStrLn $ "Part 2: " ++ show (sum $ map (makeScore freq) lst1)
