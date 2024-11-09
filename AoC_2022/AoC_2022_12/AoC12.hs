----------------------------------------------------
--              Advent of Code 2022               --
--        Day 12: Hill Climbing Algorithm         --
--            Solution by Lorin Lange             --
----------------------------------------------------

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module AoC12 where

import qualified Data.Map.Strict as Map
import qualified Queue as Q
import Data.Char ( ord )

type Pos = (Int, Int)

type Queue = Q.Queue Pos

data Color = White | Grey | Black
    deriving (Show, Eq)

data Info = Info
    { char  :: Char
    , label :: Color
    , dist  :: Int }
    deriving Show

type Graph = Map.Map Pos Info

bfs :: Graph -> (Pos -> [Pos]) -> Pos -> Int
bfs g n s =
    let q  = Q.enqueue Q.emptyQueue s
        v  = (g Map.! s) { label = Grey, dist = 0 }
        g' = Map.insert s v g
    in bfsHelper q n g'

bfsHelper :: Queue -> (Pos -> [Pos]) -> Graph -> Int
bfsHelper q nexts g =
    case Q.dequeue q of
        Nothing        -> maxBound
        (Just (p, q')) -> if (g Map.! p).char == 'E'
                            then (g Map.! p).dist
                            else let paths = filter (\p' -> (g Map.! p').label == White) $ nexts p
                                     (g', q'') = updateGraph p paths g q'
                                     info = (g Map.! p) { label = Black }
                                     graph = Map.insert p info g'
                                 in bfsHelper q'' nexts graph

updateGraph :: Pos -> [Pos] -> Graph -> Queue -> (Graph, Queue)
updateGraph _ [] g q     = (g, q)
updateGraph p (x:xs) g q 
    | isPossible p x g = 
        let info = (g Map.! x) { label = Grey, dist = (g Map.! p).dist + 1 }
            g'   = Map.insert x info g
            q'   = Q.enqueue q x
        in updateGraph p xs g' q'
    | otherwise        = updateGraph p xs g q

isPossible :: Pos -> Pos -> Graph -> Bool
isPossible is target g = 
    compareChars (g Map.! is).char (g Map.! target).char

compareChars :: Char -> Char -> Bool
compareChars is target = ord' target <= ord' is + 1
    where ord' 'S' = ord 'a'; ord' 'E' = ord 'z'; ord' a = ord a

nextsH :: Pos -> Pos -> [Pos]
nextsH (maxR, maxC) (r, c) = filter
    (\(r', c') -> r' < maxR && c' < maxC && r' >= 0 && c' >= 0)
    [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]

findStart :: Graph -> Pos
findStart = fst . head . findHelper 'S'

findStarts :: Graph -> [Pos]
findStarts = map fst . findHelper 'a'

findHelper :: Char -> Graph -> [(Pos, Info)]
findHelper c = filter ((== c) . char . snd) . Map.toList

makeGraph :: [String] -> Graph
makeGraph = Map.fromList . concatMap helper . zip [0..]
    where node c = Info { char = c, label = White, dist = maxBound }
          helper (r, s) = zipWith (\c ch -> ((c, r), node ch)) [0..] s

main :: IO()
main = do
    input <- lines <$> readFile "test_input.txt"
    let nexts = nextsH (length $ head input, length input)
    let graph = makeGraph input

    let res1 = bfs graph nexts (findStart graph)
    putStrLn $ "Part 1: " ++ show res1

    let res2 = minimum $ map (bfs graph nexts) (findStarts graph)
    putStrLn $ "Part 2: " ++ show res2
