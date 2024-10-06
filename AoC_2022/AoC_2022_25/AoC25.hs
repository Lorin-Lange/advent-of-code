----------------------------------------------------
--              Advent of Code 2022               --
--            Day 25: Full of Hot Air             --
--            Solution by Lorin Lange             --
----------------------------------------------------

module AoC25 where

getInput :: IO [String]
getInput = lines <$> readFile "./input.txt"

data SNAFUSymbol = 
  SDMinus | SMinus | SNAFU0 | SNAFU1 | SNAFU2

type SNAFU = [SNAFUSymbol]

snafuToDecimal :: SNAFU -> Integer
snafuToDecimal snafu = 
    let ufans = reverse snafu 
    in sum $ zipWith (curry convert) ufans [0..]
    where convert (SDMinus,i) = -2 * 5^i
          convert (SMinus, i) = -1 * 5^i
          convert (SNAFU0, i) =  0 * 5^i
          convert (SNAFU1, i) =  1 * 5^i
          convert (SNAFU2, i) =  2 * 5^i

snafuToString :: SNAFU -> String
snafuToString []          = ""
snafuToString (SDMinus:r) = '=' : snafuToString r
snafuToString (SMinus :r) = '-' : snafuToString r
snafuToString (SNAFU0 :r) = '0' : snafuToString r
snafuToString (SNAFU1 :r) = '1' : snafuToString r
snafuToString (SNAFU2 :r) = '2' : snafuToString r

decimalToSNAFU :: Integer -> SNAFU
decimalToSNAFU 0 = []
decimalToSNAFU i = 
  let (i', r) = (i + 2) `divMod` 5 
  in decimalToSNAFU i' ++ [c r]
    where c 0 = SDMinus
          c 1 = SMinus
          c 2 = SNAFU0
          c 3 = SNAFU1
          c 4 = SNAFU2

parseSNAFU :: String -> SNAFU
parseSNAFU []      = [] 
parseSNAFU ('=':r) = SDMinus : parseSNAFU r
parseSNAFU ('-':r) = SMinus  : parseSNAFU r
parseSNAFU ('0':r) = SNAFU0  : parseSNAFU r
parseSNAFU ('1':r) = SNAFU1  : parseSNAFU r
parseSNAFU ('2':r) = SNAFU2  : parseSNAFU r

main :: IO ()
main = do
    input <- getInput
    print $ sum . map (snafuToDecimal . parseSNAFU) $ input
    print "Result of part one"
    print . snafuToString . decimalToSNAFU . 
      sum . map (snafuToDecimal . parseSNAFU) $ input