----------------------------------------------------
--              Advent of Code 2023               --
--               Day 3: Gear Ratios               --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import qualified Data.Set as Set
import Data.Char (isDigit)

data Info = Info {
      start  :: Int
    , end    :: Int
    , line   :: Int
    , number :: Int
} deriving Show

isAdjacent :: Set.Set (Int, Int) -> Info -> Bool
isAdjacent set info = 
    let above   = map (\x -> (x, line info - 1)) [start info - 1..end info + 1]
        beneath = map (\x -> (x, line info + 1)) [start info - 1..end info + 1]
        middle  = [(start info - 1, line info), (end info + 1, line info)]
        lst     = above ++ beneath ++ middle
    in any id $ map (\t -> Set.member t set) lst

getCoordinates :: [String] -> [Info]
getCoordinates lst = concatMap (\(s, l) -> getInfos s l 0) $ zip lst [0..]

getInfos :: String -> Int -> Int -> [Info]
getInfos ""       _ _ = []
getInfos ('.':xs) l c = getInfos xs l $ c + 1
getInfos lst@(x:xs) l c | isDigit x = let number = takeWhile isDigit lst
                                          rest   = drop (length number) lst
                                      in Info { start = c, 
                                                end = c + length number - 1, 
                                                line = l, 
                                                number = read number } : 
                                                getInfos rest l (c + length number)
                        | otherwise = getInfos xs l $ c + 1

getSpecificSymbols :: [String] -> (Char -> Bool) -> Set.Set (Int, Int)
getSpecificSymbols lst f = Set.unions . map (\(s, l) -> getSymbolsH s l) $ zip lst [0..]
    where getSymbolsH s y = 
            Set.fromList $ map (\(c,x) -> (x,y)) 
                         $ filter (\(c,_) -> f c) 
                         $ zip s [0..]

getSymbols :: [String] -> Set.Set (Int, Int)
getSymbols lst = getSpecificSymbols lst $ \c -> c /= '.' && not (isDigit c)

getStars :: [String] -> Set.Set (Int, Int)
getStars lst = getSpecificSymbols lst $ \c -> c == '*'

main :: IO()
main = do
    file <- readFile "input.txt"
    let input = lines file
    let set   = getSymbols input
    let infos = getCoordinates input

    let partOne = sum $ map number $ filter (isAdjacent set) infos
    print partOne

    let stars = map (\t -> Set.fromList [t]) $ Set.toList $ getStars input

    let partTwo = sum $ map (\l -> product $ map number l) $ filter (\l -> length l == 2) $ map (\s -> filter (isAdjacent s) infos) stars
    print partTwo