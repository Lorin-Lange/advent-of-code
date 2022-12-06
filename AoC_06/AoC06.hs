----------------------------------------------------
--                                                --
--              Advent of Code 2022               --
--             Day 6: Tuning Trouble              --
--            Solution by Lorin Lange             --
--                                                --
----------------------------------------------------

module AoC06 where

getInput :: IO String
getInput = readFile "./input.txt"

checkString :: String -> Int -> Int -> Int
checkString str i l =
    let isValid = isDifferent $ checkCharacters str range
        range   = [0+i..l+i-1]
    in if isValid then maximum range + 1 else checkString str (i+1) l
    where checkCharacters str = map (str !!)

isDifferent :: String -> Bool
isDifferent str = 
    all (\((_, c1), (_, c2)) -> c1 /= c2) $ 
    filter (\((i1, _), (i2, _)) -> i1 /= i2) $ isDifferentH str
    where isDifferentH :: String -> [((Int, Char), (Int, Char))]
          isDifferentH str = do let s = zip [0..] str
                                c1 <- s
                                c2 <- s
                                return (c1, c2)

main :: IO()
main = do
    input <- getInput
    print "Result of part one"
    print $ checkString input 0 4
    print "Result of part two"
    print $ checkString input 0 14