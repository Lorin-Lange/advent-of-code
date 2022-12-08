
module AoC09 where

getInput :: IO [String]
getInput = lines <$> readFile "./input.txt"

main :: IO ()
main = do
    input <- getInput
    print "Result of part one"
    print "Result of part two"