----------------------------------------------------
--              Advent of Code 2022               --
--            Day 25: Full of Hot Air             --
--            Solution by Lorin Lange             --
----------------------------------------------------

module AoC25 where

toDecimal :: [Integer] -> Integer
toDecimal snafu = sum $
  zipWith (\s i -> s * 5^i) (reverse snafu) [0..]

parse :: String -> [Integer]
parse = map c where 
  c '=' = -2; c '-' = -1; c '0' = 0; c '1' = 1; c '2' = 2

decimalTo :: Integer -> String
decimalTo 0 = []
decimalTo i = let (i', r) = (i + 2) `divMod` 5 
              in decimalTo i' ++ [c r]
  where c 0 = '='; c 1 = '-'; c 2 = '0'; c 3 = '1'; c 4 = '2'

main :: IO ()
main = do
    input <- lines <$> readFile "./input.txt"
    let res = decimalTo . sum . map (toDecimal . parse) $ input
    putStrLn $ "Result: " ++ res