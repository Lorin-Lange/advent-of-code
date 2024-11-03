----------------------------------------------------
--              Advent of Code 2022               --
--             Day 6: Tuning Trouble              --
--            Solution by Lorin Lange             --
----------------------------------------------------

module AoC06 where

checkString :: String -> Int -> Int -> Int
checkString str i len | isValid   = maximum range + 1 
                      | otherwise = checkString str (i+1) len
    where isValid = isDifferent $ map (str !!) range
          range   = [i..len+i-1]

isDifferent :: String -> Bool
isDifferent = all (\((_, c1), (_, c2)) -> c1 /= c2) 
            . filter (\((i1, _), (i2, _)) -> i1 /= i2)
            . \s -> zip [0..] s >>= 
                \c1 -> zip [0..] s >>= 
                    \c2 -> return (c1, c2)

main :: IO()
main = do
    input <- readFile "./input.txt"
    putStrLn $ "Result of part 1: " ++ show (checkString input 0 4)  -- 1109
    putStrLn $ "Result of part 2: " ++ show (checkString input 0 14) -- 3965
