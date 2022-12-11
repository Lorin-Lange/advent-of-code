----------------------------------------------------
--                                                --
--              Advent of Code 2022               --
--          Day 11: Monkey in the Middle          --
--            Solution by Lorin Lange             --
--                                                --
----------------------------------------------------

module AoC11 where

import Data.List.Split ( splitOn , chunksOf )
import Control.Monad.State (get, put, evalState)
import Control.Monad.State.Lazy (State)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.List ( sort )

type MonkeyId = Int

type Counter = Vector Int

type MonkeyState = (Vector Monkey, Counter)

data Monkey = Monkey
    { monkeyId  :: MonkeyId
    , items     :: [Int]
    , operation :: Int -> Int
    , test      :: Int -> Bool
    , ifTrue    :: MonkeyId
    , ifFalse   :: MonkeyId }

getInput :: IO [String]
getInput = lines <$> readFile "./input.txt"

getId :: String -> MonkeyId
getId str = read $ init $ drop 7 str

getItems :: String -> [Int]
getItems str = map read $ splitOn ", " (drop 18 str)

getOperation :: String -> (Int -> Int)
getOperation str = let s = drop 23 str in opH s
    where opH :: String -> (Int -> Int)
          opH ('+':' ':"old") = \i -> i + i
          opH ('*':' ':"old") = \i -> i * i
          opH ('+':' ':xs) = \i -> i + read xs
          opH ('*':' ':xs) = \i -> i * read xs

getTest:: String -> (Int -> Bool)
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

playRounds :: Int -> Int -> State MonkeyState MonkeyState
playRounds 0 _ = get
playRounds r i = do
    (mvec, c) <- get
    case mvec V.!? i of
        Nothing -> playRounds (r-1) 0
        (Just m) -> do
            if null $ items m
            then playRounds r (i+1)
            else do let n    = c V.! i
                    let newC = V.update c $ V.singleton (i, n+1)
                    let (nm, (mId, res)) = doMonkeyBusiness m
                    let newMVec' = V.update mvec $ V.singleton (i, nm)
                    let um = mvec V.! mId
                    let umm = um { items = items um ++ [res] }
                    let newMVec = V.update newMVec' $ V.singleton (mId, umm)
                    put (newMVec, newC)
                    playRounds r i

doMonkeyBusiness :: Monkey -> (Monkey, (MonkeyId, Int))
doMonkeyBusiness m = let n    = operation m $ head $ items m
                         newN = n `div` 3
                         it   = tail $ items m
                         newM = m { items = it }
                         mId  = if test m newN then ifTrue m else ifFalse m
                     in (newM, (mId, newN))

main :: IO ()
main = do
    input <- getInput
    let monkeyState = parseMonkeys input
    print "Result of part one"
    let ms = evalState (playRounds 20 0) monkeyState
    print $ map items $ V.toList $ fst ms

    let res = product $ take 2 $ reverse $ sort $ V.toList $ snd ms
    print res
    print "Result of part two"