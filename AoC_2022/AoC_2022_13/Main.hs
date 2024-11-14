----------------------------------------------------
--              Advent of Code 2022               --
--            Day 13: Distress Signal             --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Main where

import Data.Maybe ( fromJust )
import Data.List ( sort, find )
import Text.Megaparsec ( Parsec, errorBundlePretty, parse, between, sepBy, (<|>) )
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void ( Void )
import Data.Either (fromRight)

data Packet = P [Packet] | I Int
    deriving (Show, Eq)

instance Ord Packet where
  compare (I i)   (I j) = compare i j
  compare ps    i@(I _) = compare ps $ P [i]
  compare i@(I _) qs    = compare (P [i]) qs
  compare (P ps) (P qs) = compare ps qs

type Parser = Parsec Void String

parseInput :: Parser [(Packet, Packet)]
parseInput = tuple `sepBy` newline
  where tuple   = (,) <$> packet <* newline <*> packet <* newline
        packet  = packetP <|> (I <$> decimal)
        packetP = P <$> between (char '[') (char ']') (packet `sepBy` char ',')

filterList :: [(Int, (Packet, Packet))] -> [Int]
filterList = map fst . filter (\(_, (p1, p2)) -> p1 < p2)

dividerIndex :: Packet -> [Packet] -> Int
dividerIndex p = fst . fromJust . find ((p ==) . snd) . zip [1..] . sort

main :: IO()
main = do lst <- fromRight [] . parse parseInput "" <$> readFile "input.txt"

          putStrLn $ "Part 1: " ++ show (sum $ filterList $ zip [1..] lst)

          let addL = [P [P [I 2]], P [P [I 6]]]
          let newLst = sort $ addL ++ concatMap (\(p1, p2) -> [p1, p2]) lst
          putStrLn $ "Part 2: " ++ show (product $ map (`dividerIndex` newLst) addL)
