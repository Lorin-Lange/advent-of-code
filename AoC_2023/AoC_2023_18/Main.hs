----------------------------------------------------
--              Advent of Code 2023               --
--            Day 18: Lavaduct Lagoon             --
--            Solution by Lorin Lange             --
----------------------------------------------------

import Data.List.Split ( splitOn )
import Numeric ( readHex )

getCoordinates :: [(Char, Int)] -> (Int, Int) -> [(Int, Int)]
getCoordinates []           coord = [coord]
getCoordinates ((c, n):xs) (x, y) = 
    (x, y) : getCoordinates xs (x + dx * n, y + dy * n)
    where (dx, dy) = coo c
          coo 'U' = ( 0, -1); coo 'D' = (0, 1)
          coo 'L' = (-1,  0); coo 'R' = (1, 0)

determinant :: Num a => (a, a) -> (a, a) -> a
determinant (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

shoelaceFormula :: [(Int, Int)] -> Double
shoelaceFormula lst = (0.5 *) 
                    . fromIntegral 
                    . sum 
                    . zipWith determinant lst $ tail lst ++ [head lst]

picksTheorem :: Double -> Double -> Double
picksTheorem area boundary = interior + boundary
    where interior = area - boundary / 2 + 1

parseInput1 :: String -> (Char, Int)
parseInput1 (c:_:xs) = (c, read . head $ splitOn " " xs)

parseInput2 :: String -> (Char, Int)
parseInput2 (c:_:xs) = (cc $ last n, i)
    where [(i, _)] = readHex $ init n
          n = init $ splitOn "#" xs !! 1
          cc '2' = 'L'; cc '0' = 'R'
          cc '3' = 'U'; cc '1' = 'D'

computeSolution :: [String] -> (String -> (Char, Int)) -> Integer
computeSolution inp parse = floor $ picksTheorem area boundary
    where lst      = map parse inp
          area     = shoelaceFormula $ getCoordinates lst (0, 0)
          boundary = fromIntegral $ sum $ map snd lst

main :: IO()
main = do inp <- lines <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ show (computeSolution inp parseInput1)
          putStrLn $ "Part 2: " ++ show (computeSolution inp parseInput2)
