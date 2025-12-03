----------------------------------------------------
--              Advent of Code 2025               --
--                  Day 3: Lobby                  --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main (main) where

bestN :: Int -> String -> Integer
bestN n = read . go n
  where go 0 _  = []
        go n xs = c : go (n - 1) rest
          where c           = maximum $ take (length xs - n + 1) xs
                (_, _:rest) = break (== c) xs

main :: IO()
main = do input <- lines <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (sum $ map (bestN  2) input)
          putStrLn $ "Part 2: " ++ show (sum $ map (bestN 12) input)
