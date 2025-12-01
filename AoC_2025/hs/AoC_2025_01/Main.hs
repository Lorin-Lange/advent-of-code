----------------------------------------------------
--              Advent of Code 2025               --
--             Day 1: Secret Entrance             --
--            Solution by Lorin Lange             --
----------------------------------------------------

parse :: String -> Int
parse ('L':n) = negate $ read n
parse ('R':n) = read n
parse _       = error "invalid format"

rotation :: Integral a => a -> a -> a
rotation n m = (n + m) `mod` 100

moves :: [Int] -> [Int]
moves = scanl rotation 50

main :: IO()
main = do input <- map parse . lines <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (sum [1 | n <- moves input, n == 0])
