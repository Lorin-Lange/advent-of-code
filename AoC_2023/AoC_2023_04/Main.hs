----------------------------------------------------
--              Advent of Code 2023               --
--              Day 4: Scratchcards               --
--            Solution by Lorin Lange             --
----------------------------------------------------

import qualified Data.Set as Set
import Data.List.Split ( splitOn )

type Card = (Int, [Int], Set.Set Int)

parseCard :: String -> Card
parseCard s =
    let inp   = splitOn ":" s
        id'   = read $ drop 5 (head inp)
        cards = splitOn "|" (inp !! 1)
    in (id', getList $ head cards, Set.fromList $ getList $ cards !! 1)
    where getList = map read $ filter (/= "") $ splitOn " "

numOfMatches :: Set.Set Int -> [Int] -> Int
numOfMatches set = length . filter (\x -> Set.member x set)

calculate1 :: Card -> Int
calculate1 (_, lst, set) = 
    let n = numOfMatches set lst
    in if n == 0 then 0 else 2^(n-1)

calculate2 :: [Card] -> [(Int, Int, Int)]
calculate2 = calc . map (\(i, l, s) -> (i, numOfMatches s l, 1))
    where calc []                = []
          calc (tu@(_, n, m):xs) =
              let next = map (\(id, no, t) -> (id, no, t + m)) $ take n xs
                  rest = drop n xs
              in tu : calc (next ++ rest)

main :: IO()
main = do
    input <- lines <$> readFile "input.txt"

    let cards = map parseCard input

    let partOne = sum $ map calculate1 cards
    putStrLn $ "Part 1: " ++ show partOne
    
    let partTwo = sum $ map (\(_, _, t) -> t) $ calculate2 cards
    putStrLn $ "Part 2: " ++ show partTwo
