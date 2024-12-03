----------------------------------------------------
--              Advent of Code 2024               --
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
import Data.Composition ( (.:) )

type Parser = Parsec Void String

data Mul = Mul Integer | Do | Dont
    deriving Show
 
parseInput1 :: Parser [Mul]
parseInput1 = many $ try $ skipManyTill anySingle $ try mulCmd
    where mulCmd = between (string "mul(") (char ')') mul
          mul = Mul .: (*) <$> L.decimal <* char ',' <*> L.decimal

parseInput2 :: Parser [Mul]
parseInput2 = many $ try $ skipManyTill anySingle $ try cmd
    where cmd    = mulCmd <|> do' <|> dont
          mulCmd = between (string "mul(") (char ')') mul
          mul  = Mul .: (*) <$> L.decimal <* char ',' <*> L.decimal
          do'  = string "do()" >> return Do
          dont = string "don't()" >> return Dont

filterMul :: [Mul] -> [Mul]
filterMul lst = filterMulH lst True
    where filterMulH []        _ = []
          filterMulH (Do:xs)   _ = filterMulH xs True
          filterMulH (Dont:xs) _ = filterMulH xs False
          filterMulH (x:xs)    b = if b then x : filterMulH xs b else filterMulH xs b

main :: IO()
main = do input <- readFile "input.txt"

          let part1 = fromRight [] $ parse parseInput1 "" input
          putStrLn $ "Part 1: " ++ show (sum $ map (\(Mul v) -> v) part1)

          let part2 = fromRight [] $ parse parseInput2 "" input
          putStrLn $ "Part 2: " ++ show (sum $ map (\(Mul v) -> v) $ filterMul part2)
