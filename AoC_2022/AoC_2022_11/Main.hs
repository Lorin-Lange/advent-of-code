----------------------------------------------------
--              Advent of Code 2022               --
--          Day 11: Monkey in the Middle          --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.List.Split ( splitOn , chunksOf )
import Control.Monad.State (get, put, evalState)
import Control.Monad.State.Lazy (State)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.List ( sort, sortBy )
import Data.Ord (comparing, Down (Down))

type MonkeyId = Int

type Counter = Vector Int

type MonkeyState = (Vector Monkey, Counter)

type MonkeyBusiness = Monkey -> (Monkey, (MonkeyId, Integer))

data Monkey = Monkey
    { monkeyId  :: MonkeyId
    , items     :: [Integer]
    , operation :: Integer -> Integer
    , test      :: Integer -> Bool
    , ifTrue    :: MonkeyId
    , ifFalse   :: MonkeyId }

getId :: String -> MonkeyId
getId str = read $ init $ drop 7 str

getItems :: String -> [Integer]
getItems str = map read $ splitOn ", " (drop 18 str)

getOperation :: String -> (Integer -> Integer)
getOperation str = opH $ drop 23 str
    where opH ('+':' ':"old") = \i -> i + i
          opH ('*':' ':"old") = \i -> i * i
          opH ('+':' ':xs)    = \i -> i + read xs
          opH ('*':' ':xs)    = \i -> i * read xs

getTest:: String -> (Integer -> Bool)
getTest str = let ii = read (drop 21 str) in (\i -> i `mod` ii == 0)

getIfTrue :: String -> MonkeyId
getIfTrue str = read $ drop 29 str

getIfFalse :: String -> MonkeyId
getIfFalse str = read $ drop 30 str

parseMonkey :: [String] -> Monkey
parseMonkey lst = Monkey
    { monkeyId  = getId        $ lst !! 0
    , items     = getItems     $ lst !! 1
    , operation = getOperation $ lst !! 2
    , test      = getTest      $ lst !! 3
    , ifTrue    = getIfTrue    $ lst !! 4
    , ifFalse   = getIfFalse   $ lst !! 5 }

parseMonkeys :: [String] -> MonkeyState
parseMonkeys str = let lst = map parseMonkey $ chunksOf 7 str in
    (V.fromList lst, V.replicate (length lst) 0)

playRounds :: Int -> Int -> MonkeyBusiness -> State MonkeyState MonkeyState
playRounds 0 _ _  = get
playRounds r i mb = do
    (mvec, c) <- get
    case mvec V.!? i of
        Nothing -> playRounds (r-1) 0 mb
        (Just m) -> do
            if null $ items m
            then playRounds r (i+1) mb
            else do let n    = c V.! i
                    let newC = V.update c $ V.singleton (i, n+1)
                    let (nm, (mId, res)) = mb m
                    let newMVec' = V.update mvec $ V.singleton (i, nm)
                    let um = mvec V.! mId
                    let umm = um { items = items um ++ [res] }
                    let newMVec = V.update newMVec' $ V.singleton (mId, umm)
                    put (newMVec, newC)
                    playRounds r i mb

doMonkeyBusiness :: (Integer -> Integer) -> MonkeyBusiness
doMonkeyBusiness f m = let n    = operation m $ head $ items m
                           newN = f n
                           it   = tail $ items m
                           newM = m { items = it }
                           mId  = if test m newN then ifTrue m else ifFalse m
                       in (newM, (mId, newN))

divisor :: Integer
divisor = product [13, 19, 5, 2, 17, 11, 7, 3]

main :: IO ()
main = do
    monkeyState <- parseMonkeys . lines <$> readFile "input.txt"

    print "Result of part one"
    let ms = evalState (playRounds 20 0 (doMonkeyBusiness (`div` 3))) monkeyState
    let res1 = product $ take 2 $ sortBy (comparing Down) (V.toList $ snd ms)
    print res1

    print "Result of part two"
    let ms = evalState (playRounds 10000 0 (doMonkeyBusiness (`mod` divisor))) monkeyState
    let res2 = product $ take 2 $ sortBy (comparing Down) (V.toList $ snd ms)
    print res2