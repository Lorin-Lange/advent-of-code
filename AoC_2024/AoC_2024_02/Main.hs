----------------------------------------------------
--              Advent of Code 2024               --
--            Day 2: Red-Nosed Reports            --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

isSafe :: [Int] -> Bool
isSafe lst = monotonic && inRange
    where ds = zipWith (-) (init lst) (tail lst)
          monotonic = all (> 0) ds || all (< 0) ds
          inRange = all (\x -> abs x <= 3) ds

problemDampener :: [Int] -> [Int]
problemDampener lst = lst

main :: IO()
main = do
    input <- map words . lines <$> readFile "test_input.txt"
    let lst = map (map read) input

    putStrLn $ "Part 1: " ++ show (length $ filter isSafe lst)
    --putStrLn $ "Part 2: " ++ show (length $ filter isSafe $ map problemDampener lst)
