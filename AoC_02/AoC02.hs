----------------------------------------------------
--                                                --
--              Advent of Code 2022               --
--           Day 2: Rock Paper Scissors           --
--            Solution by Lorin Lange             --
--                                                --
----------------------------------------------------

module AoC02 where

import Data.List.Split ( endBy )
import Data.Bifunctor ( bimap )

type Score = Integer

data Shape = Rock | Paper | Scissors
   deriving (Show, Eq)

data Result = Win | Loss | Draw
   deriving (Show, Eq)

scoreOfShape :: Shape -> Score
scoreOfShape Rock     = 1
scoreOfShape Paper    = 2
scoreOfShape Scissors = 3

scoreOfRes :: Result -> Score
scoreOfRes Win  = 6
scoreOfRes Loss = 0
scoreOfRes Draw = 3

calculateResult :: Shape -> Shape -> Result
calculateResult Rock Scissors  = Win
calculateResult Scissors Paper = Win
calculateResult Paper Rock     = Win
calculateResult opponent you | opponent == you = Draw
                             | otherwise = Loss

parseShape1 :: String -> Shape
parseShape1 "X" = Rock
parseShape1 "Y" = Paper
parseShape1 "Z" = Scissors
parseShape1 "A" = Rock
parseShape1 "B" = Paper
parseShape1 "C" = Scissors

parseShape2 :: String -> Result
parseShape2 "X" = Loss
parseShape2 "Y" = Draw
parseShape2 "Z" = Win

calculateScore :: (Shape, Shape) -> Score
calculateScore (opp, p) = scoreOfShape p + scoreOfRes (calculateResult p opp)

getInput :: IO [(String, String)]
getInput = do lst <- lines <$> readFile "./input.txt"
              return $ map ((\ l -> (head l, head $ tail l)) . endBy " ") lst

parseInput1 :: [(String, String)] -> [(Shape, Shape)]
parseInput1 = map $ bimap parseShape1 parseShape1

parseInput2 :: [(String, String)] -> [(Shape, Result)]
parseInput2 = map $ bimap parseShape1 parseShape2

calculateMove :: (Shape, Result) -> (Shape, Shape)
calculateMove (s, Draw)        = (s, s)
calculateMove (Rock, Win)      = (Rock, Paper)
calculateMove (Paper, Win)     = (Paper, Scissors)
calculateMove (Scissors, Win)  = (Scissors, Rock)
calculateMove (Rock, Loss)     = (Rock, Scissors)
calculateMove (Paper, Loss)    = (Paper, Rock)
calculateMove (Scissors, Loss) = (Scissors, Paper)

main :: IO ()
main = do
    lst <- getInput
    print "Result of part one"
    print $ sum $ map calculateScore (parseInput1 lst)
    print "Result of part two"
    print $ sum $ map (calculateScore . calculateMove) (parseInput2 lst)