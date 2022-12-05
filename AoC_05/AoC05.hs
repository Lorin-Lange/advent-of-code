----------------------------------------------------
--                                                --
--              Advent of Code 2022               --
--              Day 5: Supply Stacks              --
--            Solution by Lorin Lange             --
--                                                --
----------------------------------------------------

module AoC05 where

import Data.List (transpose)
import Data.String.Utils ( strip )
import Control.Monad.State.Lazy (State)
import Control.Monad.State (get, put, evalState)
import qualified Data.Map as M
import Data.Maybe (fromJust)

type Stack = String

data Move = Move 
   { n    :: Int
   , from :: Int
   , to   :: Int }
   deriving Show

containsBracket :: String -> Bool
containsBracket s = not $ '[' `elem` s || ']' `elem` s || all (== ' ') s

getInput :: IO [String]
getInput = lines <$> readFile "./input.txt"

getMoves :: [String] -> [Move]
getMoves = map parseMove

parseMove :: String -> Move
parseMove s = let s0 = drop 5 s
                  (n, s1) = getNumber s0 ""
                  (f, s2) = getNumber (drop 6 s1) ""
                  (t, _)  = getNumber (drop 4 s2) "" 
              in Move { n = n, from = f, to = t }

getNumber :: String -> String -> (Int, String)
getNumber [x] s | x `elem` ['0'..'9'] = (read (s ++ [x]), "") 
                | otherwise = (read s, "")
getNumber st@(x:xs) s | x `elem` ['0'..'9'] = getNumber xs (s ++ [x])
                      | otherwise = (read s, st)

type Stacks = M.Map Int Stack

safeTail :: Stack -> Stack
safeTail []     = []
safeTail (x:xs) = xs

oneMove :: Stacks -> Int -> Int -> Stacks
oneMove st from to = let f0 = fromJust $ M.lookup from st
                         t0 = fromJust $ M.lookup to st
                         m0 = if null f0 then st else M.insert to (head f0 : t0) st
                     in M.insert from (safeTail f0) m0

applyMove :: Move -> Stacks -> Stacks
applyMove Move { n = 0, from = _, to = _ } l = l
applyMove Move { n = n, from = f, to = t } l = 
      let stacks = oneMove l f t in
      applyMove Move { n = n - 1, from = f, to = t } stacks

applyMoves :: Move -> Stacks -> Stacks
applyMoves Move { n = n, from = f, to = t } st = 
      let f0 = fromJust $ M.lookup f st
          t0 = fromJust $ M.lookup t st
          m0 = M.insert t (take n f0 ++ t0) st
      in M.insert f (drop n f0) m0

rearrange :: [Move] -> (Move -> Stacks -> Stacks) -> State Stacks Stacks
rearrange []     _ = get
rearrange (x:xs) f = do
    st <- get
    put $ f x st
    rearrange xs f

main :: IO()
main = do
    i <- getInput
    let stackData = map strip $ filter containsBracket $ transpose $ take 10 i
    let stacks = M.fromList $ zip [1..9] (map init stackData)
    let instructionData = drop 10 i
    let moves = getMoves instructionData
    print "Result of part one"
    let res1 = M.toList $ evalState (rearrange moves applyMove) stacks
    print $ map (\(_,s) -> head s) res1
    print "Result of part two"
    let res2 = M.toList $ evalState (rearrange moves applyMoves) stacks
    print $ map (\(_,s) -> head s) res2