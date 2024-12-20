----------------------------------------------------
--              Advent of Code 2024               --
--              Day 3: Mull It Over               --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Text.Megaparsec ( Parsec, parse, between, 
    skipManyTill, many, MonadParsec (try), anySingle, (<|>) )
import Text.Megaparsec.Char ( char, string )
import Data.Void ( Void )
import Data.Either ( fromRight )
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Composition ( (.:) )

type Parser = Parsec Void String

data Mul = Mul Integer | Do | Dont
    deriving Show

parseInput :: Bool -> Parser [Mul]
parseInput b = many . try . skipManyTill anySingle $ try cmd
    where cmd    = if b then mulCmd <|> do_ <|> don't else mulCmd
          mulCmd = between (string "mul(") (char ')') mul
          mul    = Mul .: (*) <$> L.decimal <* char ',' <*> L.decimal
          do_    = string "do()" >> return Do
          don't  = string "don't()" >> return Dont

filterMul :: [Mul] -> [Mul]
filterMul lst = filterMulH lst True
    where filterMulH []        _ = []
          filterMulH (Do:xs)   _ = filterMulH xs True
          filterMulH (Dont:xs) _ = filterMulH xs False
          filterMulH (x:xs)    b | b         = x : filterMulH xs b 
                                 | otherwise =     filterMulH xs b

solve :: ([Mul] -> [Mul]) -> Bool -> String -> Integer
solve f b = sum . map (\(Mul v) -> v) . f . fromRight [] . parse (parseInput b) ""

main :: IO()
main = do input <- readFile "input.txt"

          putStrLn $ "Part 1: " ++ show (solve id False input)
          putStrLn $ "Part 2: " ++ show (solve filterMul True input)
