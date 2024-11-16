----------------------------------------------------
--              Advent of Code 2022               --
--               Day 9: Rope Bridge               --
--            Solution by Lorin Lange             --
----------------------------------------------------

module AoC09 where

import qualified Data.Set as S

type Cor = (Int, Int)

type UniqueTailPos = S.Set Cor

data Dir = U Int | D Int | L Int | R Int
  deriving (Show, Read, Eq)

testInput :: String
testInput = "R 4\n\
            \U 4\n\
            \L 3\n\
            \D 1\n\
            \R 4\n\
            \D 1\n\
            \L 5\n\
            \R 2"

getInput :: IO [String]
getInput = lines <$> readFile "./input.txt"

convert :: String -> Dir
convert ('U':_:xs) = U $ read xs
convert ('D':_:xs) = D $ read xs
convert ('L':_:xs) = L $ read xs
convert ('R':_:xs) = R $ read xs

move :: Dir -> Cor -> (Cor, Dir)
move (U i) (x, y) = ((x, y + 1), U (i-1))
move (D i) (x, y) = ((x, y - 1), D (i-1))
move (L i) (x, y) = ((x - 1, y), L (i-1))
move (R i) (x, y) = ((x + 1, y), R (i-1))

adjustTail :: Cor -> Cor -> Cor
adjustTail (hx, hy) (tx, ty)
  | abs dx <= 1 && abs dy <= 1 = (tx, ty)
  | abs dx > abs dy            = (hx - signum dx, hy)
  | abs dx < abs dy            = (hx, hy - signum dy)
  | otherwise                  = (hx - signum dx, hy - signum dy)
  where
    dx = hx - tx
    dy = hy - ty

isZero :: Dir -> Bool
isZero (U i) = i == 0
isZero (D i) = i == 0
isZero (R i) = i == 0
isZero (L i) = i == 0

applyMoves :: [Dir] -> Cor -> Cor -> UniqueTailPos -> UniqueTailPos
applyMoves [] _ _ s = s
applyMoves (x:xs) h t s = let (posH, newDir) = move x h
                              posT           = adjustTail posH t
                              newS           = S.insert posT s
                          in applyMoves (if isZero newDir then xs else newDir:xs) posH posT newS

main :: IO ()
main = do
    input <- getInput
    let moves = map convert input
    print "Result of part one"
    print $ S.size $ applyMoves moves (0,0) (0,0) $ S.singleton (0,0)

    print "Result of part two"
    print "TODOs"