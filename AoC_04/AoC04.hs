----------------------------------------------------
--                                                --
--              Advent of Code 2022               --
--              Day 4: Camp Cleanup               --
--            Solution by Lorin Lange             --
--                                                --
----------------------------------------------------

module AoC04 where

type Range = (Integer, Integer)

testInput :: String
testInput = "2-4,6-8\n\
            \2-3,4-5\n\
            \5-7,7-9\n\
            \2-8,3-7\n\
            \6-6,4-6\n\
            \2-6,4-8"

getInput :: IO [String]
getInput = lines <$> readFile "./input.txt"

parseInput :: [String] -> [(Range, Range)]
parseInput = map parser

parser :: String -> (Range, Range)
parser str = let (l, r)   = splitC str
                 (ll, lr) = splitH l
                 (rl, rr) = splitH r
             in ((read ll, read lr), (read rl, read rr))

splitC :: String -> (String, String)
splitC str = split str ',' False ("", "")

splitH :: String -> (String, String)
splitH str = split str '-' False ("", "")

split :: String -> Char -> Bool -> (String, String) -> (String, String)
split [] _ _ t = t 
split (x:xs) d False (l, r) | x == d = split xs d True (l, r)
                            | otherwise = split xs d False (l ++ [x], r)
split (x:xs) d True (l, r) = split xs d True (l, r ++ [x])

isContained :: (Range, Range) -> Bool
isContained ((ll, lr), (rl, rr)) = ll <= rl && rr <= lr || ll >= rl && lr <= rr

isOverlapping :: (Range, Range) -> Bool
isOverlapping ((ll, lr), (rl, rr)) = lr >= rl && ll <= rr

main :: IO()
main = do
    i <- getInput
    let input = parseInput i

    print "Result of part one"
    let tInput = parseInput $ lines testInput
    let testRes1 = length $ filter isContained tInput
    print testRes1

    let res1 = length $ filter isContained input
    print res1

    print "Result of part two"
    let testRes2 = length $ filter isOverlapping tInput
    print testRes2

    let res2 = length $ filter isOverlapping input
    print res2