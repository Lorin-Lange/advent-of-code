----------------------------------------------------
--                                                --
--              Advent of Code 2022               --
--         Day 15: Beacon Exclusion Zone          --
--            Solution by Lorin Lange             --
--                                                --
----------------------------------------------------

module Main where

import Data.List (sortBy)
import Data.Function (on)
import Data.List.Split (endBy)
import qualified Data.Set as S

data Place = S | B | I | P
   deriving (Show, Eq, Ord)

type Space = (Integer, Integer, Place)

data Pair = Pair 
    { sx :: Integer
    , sy :: Integer 
    , bx :: Integer
    , by :: Integer }
    deriving Show

parseInput :: String -> Pair
parseInput str = let l  = endBy ":" str
                     l1 = endBy "," $ l !! 0
                     l2 = endBy "," $ l !! 1
                     sx = read $ drop 12 $ l1 !! 0
                     sy = read $ drop  3 $ l1 !! 1
                     bx = read $ drop 24 $ l2 !! 0
                     by = read $ drop  3 $ l2 !! 1
                in Pair { sx, sy, bx, by }

dist :: Pair -> Integer
dist Pair { sx, sy, bx, by } = abs (sx - bx) + abs (sy - by)

impossiblePos :: [Integer] -> Pair -> S.Set Space
impossiblePos i p@Pair { sx, sy, bx, by } = let d = dist p in S.fromList
    [(x,y,f x y p) | x <- [sx-d..sx+d], y <- i, abs (sx - x) + abs (sy - y) <= d]

filterLst :: Integer -> [Pair] -> [Pair]
filterLst i  = filter (\p@Pair { sx, sy, bx, by } -> let d = dist p in sy - d <= i && i <= sy + d)

f :: Integer -> Integer -> Pair -> Place
f x y Pair { sx, sy, bx, by } | x == sx && y == sy = S
                              | x == bx && y == by = B
                              | otherwise          = I

calculate :: [Integer] -> S.Set Space -> [Pair] -> S.Set Space
calculate i = foldl $ \r x -> impossiblePos i x `S.union` r

getInput :: String -> IO [String]
getInput file = lines <$> readFile file



getMissing :: [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
getMissing []  res = res
getMissing [x] res = res
getMissing ((x1,y1):(x2,y2):xs) res | x1 == x2 && y1 == y2 - 1 = getMissing ((x2,y2):xs) res
                                    | x1 /= x2                 = getMissing ((x2,y2):xs) res
                                    | otherwise = getMissing ((x1,y1):(x1, y1 + 1):(x2,y2):xs) ((x1, y1 + 1) : res)



main :: IO ()
main = do

    testInput <- getInput "./testInput.txt"
    let testLst = filterLst 10 $ map parseInput testInput

    input <- getInput "./input.txt"
    let lst = filterLst 2000000 $ map parseInput input

    print "Result of part one"

    print "Test-input"
    print $ length $ filter (\(_, _, j) -> j == I) $ S.toList $ calculate [10] S.empty testLst

    print "Input"
    --print $ length $ filter (\(_, _, j) -> j == I) $ S.toList $ calculate 2000000 S.empty lst

    print "Result of part two"
    let filteredTestLst = filter (\Pair { sx, sy, bx, by } -> sx <= 20 && sy <= 20) testLst
    let pos = map (\(x,y,_) -> (x,y)) $ S.toList $ calculate [0..20] S.empty filteredTestLst
    let newLst = sortBy (compare `on` fst) pos
    print $ newLst
    print $ getMissing newLst []

    --let compl = S.fromList [(x, y) | x <- [0..20], y <- [0..20]]
    --print compl
    --print $ compl S.\\ pos

    --let filteredLst = filter (\Pair { sx, sy, bx, by } -> sx >= 0 && sy >= 0 && sx <= 4000000 && sy <= 4000000) lst
    --print $ S.toList $ calculate [0..4000000] S.empty filteredLst
    --print $ S.fromList [(x, y, P) | x <- [0..4000000], y <- [0..4000000]]