----------------------------------------------------
--              Advent of Code 2022               --
--            Day 25: Full of Hot Air             --
--            Solution by Lorin Lange             --
----------------------------------------------------

{-# Language LambdaCase #-}

module Main where

toDecimal :: [Integer] -> Integer
toDecimal = sum . zipWith (\i s -> s * 5 ^ i) [0..] . reverse

toSNAFU :: Integer -> String
toSNAFU i = if i == 0 then "" else toSNAFU i' ++ [c r]
  where (i', r) = (i + 2) `divMod` 5
        c = \case 0 -> '='; 1 -> '-'; 2 -> '0'; 3 -> '1'; 4 -> '2'

parse :: [String] -> [[Integer]]
parse = map . map $ \case '=' -> -2; '-' -> -1; '0' -> 0; '1' -> 1; '2' -> 2

main :: IO ()
main = do inp <- parse . lines <$> readFile "input.txt"
          putStrLn $ "Result: " ++ (toSNAFU . sum $ map toDecimal inp)
