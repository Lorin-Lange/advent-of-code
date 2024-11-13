----------------------------------------------------
--              Advent of Code 2022               --
--            Day 25: Full of Hot Air             --
--            Solution by Lorin Lange             --
----------------------------------------------------

{-# Language LambdaCase #-}

module Main where

toDecimal :: [Integer] -> Integer
toDecimal encoded = sum $ zipWith (\s i -> s * 5^i) (reverse encoded) [0..]

toSNAFU :: Integer -> String
toSNAFU 0 = ""
toSNAFU i = toSNAFU i' ++ [c r]
  where (i', r) = (i + 2) `divMod` 5
        c = \case 0 -> '='; 1 -> '-'; 2 -> '0'; 3 -> '1'; 4 -> '2'

parse :: String -> [Integer]
parse = map $ \case '=' -> -2; '-' -> -1; '0' -> 0; '1' -> 1; '2' -> 2

main :: IO ()
main = do input <- lines <$> readFile "input.txt"
          let result = toSNAFU . sum . map (toDecimal . parse) $ input
          putStrLn $ "Result: " ++ result
