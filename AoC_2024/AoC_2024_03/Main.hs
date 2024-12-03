----------------------------------------------------
--              Advent of Code 2015               --
--              Day 3: Mull It Over               --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Text.Megaparsec ( Parsec, parse, between, 
    skipManyTill, many, MonadParsec (try), anySingle )
import Text.Megaparsec.Char (char, string )
import Data.Void ( Void )
import Data.Either (fromRight)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Mul = Mul Integer Integer
    deriving Show

parseInput1 :: Parser [Mul]
parseInput1 = many $ try $ skipManyTill anySingle $ try mulCmd
    where mulCmd = between (string "mul(") (char ')') mul
          mul = Mul <$> L.decimal <* char ',' <*> L.decimal

main :: IO()
main = do lst <- readFile "test_input_1.txt"

          let part1 = fromRight [] $ parse parseInput1 "" lst
          putStrLn $ "Part 1: " ++ show (sum $ map (\(Mul n1 n2) -> n1 * n2) part1)
