----------------------------------------------------
--              Advent of Code 2023               --
--            Day 18: Lavaduct Lagoon             --
--            Solution by Lorin Lange             --
----------------------------------------------------

import Data.List.Split ( splitOn )
import Numeric ( readHex )

type Coordinate = (Int, Int)

getCoordinates :: [(Char, Int)] -> Coordinate -> [Coordinate]
getCoordinates [] coord = [coord]
getCoordinates ((c, n):xs) (x, y) = 
    (x, y) : getCoordinates xs (x + dx * n, y + dy * n)
    where (dx, dy) = coordinate c
          coordinate 'U' = ( 0, -1)
          coordinate 'D' = ( 0,  1)
          coordinate 'L' = (-1,  0)
          coordinate 'R' = ( 1,  0)

determinant :: Num a => (a, a) -> (a, a) -> a
determinant (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

shoelaceFormula :: [Coordinate] -> Double
shoelaceFormula lst = (0.5 *) 
                    . fromIntegral 
                    . sum 
                    . zipWith determinant lst $ tail lst ++ [head lst]

picksTheorem :: Double -> Double -> Double
picksTheorem area boundary =
    let interior = area - boundary / 2 + 1
    in interior + boundary

parseInput1 :: String -> (Char, Int)
parseInput1 (c:_:xs) = (c, read . head $ splitOn " " xs)

parseInput2 :: String -> (Char, Int)
parseInput2 (c:_:xs) = let n        = init $ splitOn "#" xs !! 1
                           [(i, _)] = readHex $ init n
                       in (convertChar $ last n, i)
                       where convertChar '0' = 'R'
                             convertChar '1' = 'D'
                             convertChar '2' = 'L'
                             convertChar '3' = 'U'

main :: IO()
main = do
    inp <- lines <$> readFile "input.txt"

    let lst1      = map parseInput1 inp
    let coords1   = getCoordinates lst1 (0, 0)
    let area1     = shoelaceFormula coords1
    let boundary1 = fromIntegral $ sum $ map snd lst1
    let res1      = floor $ picksTheorem area1 boundary1
    putStrLn $ "Part 1: " ++ show res1

    let lst2      = map parseInput2 inp
    let coords2   = getCoordinates lst2 (0, 0)
    let area2     = shoelaceFormula coords2
    let boundary2 = fromIntegral $ sum $ map snd lst2
    let res2      = floor $ picksTheorem area2 boundary2
    putStrLn $ "Part 2: " ++ show res2
