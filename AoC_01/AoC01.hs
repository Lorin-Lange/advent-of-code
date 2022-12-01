{- AoC01 by Lorin Lange -}

module AoC01 where

import Text.Read ( readMaybe )
import Data.List ( sort )
import Data.List.Split ( endBy )
import Data.Maybe ( fromMaybe )

getInput :: IO [Maybe Integer]
getInput = do lst <- lines <$> readFile "./input.txt"
              return $ map readMaybe lst

sum' :: [Maybe Integer] -> [Integer]
sum' lst = map (sum . map (fromMaybe 0)) $ endBy [Nothing] lst

getMaxElf :: [Integer] -> Integer
getMaxElf = maximum

getTopThreeElvesSum :: [Integer] -> Integer
getTopThreeElvesSum lst = sum $ take 3 $ reverse $ sort lst

main :: IO()
main = do
    lst <- getInput
    let sums = sum' lst
    print $ getMaxElf sums
    print $ getTopThreeElvesSum sums
