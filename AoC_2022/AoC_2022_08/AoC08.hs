----------------------------------------------------
--                                                --
--              Advent of Code 2022               --
--           Day 8: Treetop Tree House            --
--            Solution by Lorin Lange             --
--                                                --
----------------------------------------------------

module AoC08 where

import Data.Char(digitToInt)
import Data.List (transpose, zip4, zipWith4)

getInput :: IO [String]
getInput = lines <$> readFile "./input.txt"

testInput :: String
testInput = "30373\n\
            \25512\n\
            \65332\n\
            \33549\n\
            \35390"

f :: [(Int, Int, Bool)] -> [(Int, Int, Bool)]
f = scanl1 (\(i2, lm2, b2) (i1, lm1, b1) -> (i1, max i1 lm2, i1 > lm2))

zipBooleans :: ([(Int, Int, Bool)], [(Int, Int, Bool)], [(Int, Int, Bool)], [(Int, Int, Bool)]) -> [Bool]
zipBooleans (ll1, ll2, ll3, ll4) = zipWith4 (\(_, _, b1) (_, _, b2) (_, _, b3) (_, _, b4) -> b1 || b2 || b3 || b4) ll1 ll2 ll3 ll4

main :: IO ()
main = do
    input <- getInput
    print "Result of part one"
    let lst = map (map (\c -> (digitToInt c, digitToInt c, True))) input
    let l1 = map f lst
    let l2 = map (reverse . f . reverse) lst
    let l3 = transpose $ map f $ transpose lst
    let l4 = transpose $ map (reverse . f . reverse) $ transpose lst
    print . length . filter id . concatMap zipBooleans $ zip4 l1 l2 l3 l4

    print "Result of part two"