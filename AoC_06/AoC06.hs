
module AoC05 where

getInput :: IO [String]
getInput = lines <$> readFile "./input.txt"

main :: IO()
main = do
    i <- getInput
    print "Result of part one"
    print "Result of part two"