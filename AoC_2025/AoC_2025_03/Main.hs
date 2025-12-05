----------------------------------------------------
--              Advent of Code 2025               --
--                  Day 3: Lobby                  --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main (main) where

bestN :: Int -> String -> Int
bestN n = read . go n
  where go 0 _  = []
        go n xs = c : go (n - 1) r
          where c        = maximum $ take (length xs - n + 1) xs
                (_, _:r) = break (== c) xs

solve :: Int -> [String] -> Int
solve n = sum . map (bestN n)

main :: IO()
main = do input <- lines <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (solve  2 input)
          putStrLn $ "Part 2: " ++ show (solve 12 input)
