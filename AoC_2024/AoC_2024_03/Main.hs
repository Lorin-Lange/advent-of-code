----------------------------------------------------
--              Advent of Code 2015               --
--              Day 3: Mull It Over               --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Text.Megaparsec ( Parsec, parse, between, 
    skipManyTill, many, MonadParsec (try), anySingle, (<|>))
import Text.Megaparsec.Char (char, string )
import Data.Void ( Void )
import Data.Either (fromRight)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Mul = Mul Integer Integer | Do | Dont
    deriving Show
 
parseInput1 :: Parser [Mul]
parseInput1 = many $ try $ skipManyTill anySingle $ try mulCmd
    where mulCmd = between (string "mul(") (char ')') mul
          mul = Mul <$> L.decimal <* char ',' <*> L.decimal

parseInput2 :: Parser [Mul]
parseInput2 = many $ try $ skipManyTill anySingle $ try cmd
    where cmd = mulCmd <|> do' <|> dont
          mulCmd = between (string "mul(") (char ')') mul
          mul = Mul <$> L.decimal <* char ',' <*> L.decimal
          do' = string "do()" >> return Do
          dont = string "don't()" >> return Dont

filterMul :: [Mul] -> [Mul]
filterMul lst = filterMulH lst True
    where filterMulH []        _ = []
          filterMulH (Do:xs)   _ = filterMulH xs True
          filterMulH (Dont:xs) _ = filterMulH xs False
          filterMulH (x:xs) b | b = x : filterMulH xs b
                              | otherwise = filterMulH xs b

main :: IO()
main = do input <- readFile "input.txt"

          let sumOfProducts = sum . map (\(Mul n1 n2) -> n1 * n2)

          let part1 = fromRight [] $ parse parseInput1 "" input
          putStrLn $ "Part 1: " ++ show (sumOfProducts part1)

          let part2 = fromRight [] $ parse parseInput2 "" input
          putStrLn $ "Part 2: " ++ show (sumOfProducts $ filterMul part2)
