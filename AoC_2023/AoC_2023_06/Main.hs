----------------------------------------------------
--              Advent of Code 2023               --
--               Day 6: Wait For It               --
--            Solution by Lorin Lange             --
----------------------------------------------------

import Data.List.Split ( splitOn )

parseRaces :: [String] -> [(Int, Int)]
parseRaces [s1, s2] = zip (getList s1) (getList s2)
    where getList lst = map read $ words $ splitOn ":" lst !! 1

parseRace :: [String] -> (Int, Int)
parseRace [s1, s2] = (getNum s1, getNum s2)
    where getNum s = read $ filter (/= ' ') $ splitOn ":" s !! 1

numberOfWays :: (Int, Int) -> Int
numberOfWays (t, d) = 
    let b  = fromIntegral t / 2
        d' = sqrt $ b * b - fromIntegral d
    in ceiling (b + d' - 1) - floor (b - d' + 1) + 1  

main :: IO()
main = do
    input <- lines <$> readFile "input.txt"

    let firstPart = product . map numberOfWays $ parseRaces input
    putStrLn $ "Part 1: " ++ show firstPart

    let secondPart = numberOfWays $ parseRace input
    putStrLn $ "Part 2: " ++ show secondPart
