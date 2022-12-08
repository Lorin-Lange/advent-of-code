----------------------------------------------------
--                                                --
--              Advent of Code 2022               --
--         Day 7: No Space Left On Device         --
--            Solution by Lorin Lange             --
--                                                --
----------------------------------------------------

module AoC07 where

import Text.Parsec.Combinator ( many1, eof, choice ) 
import Text.Parsec.String ( Parser )
import Text.Parsec ( digit, parse, letter, ParseError )
import Text.Parsec.Prim ( runParser )
import Text.ParserCombinators.Parsec ( string, anyChar, try )
import Text.Parsec.Char ( char )
import Control.Applicative ( (<|>) )
import Data.Either ( rights )
import Control.Monad.State.Lazy (State)
import Control.Monad.State (get, put, evalState)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)

testInput :: String
testInput = "$ cd /\n\
            \$ ls\n\
            \dir a\n\
            \14848514 b.txt\n\
            \8504156 c.dat\n\
            \dir d\n\
            \$ cd a\n\
            \$ ls\n\
            \dir e\n\
            \29116 f\n\
            \2557 g\n\
            \62596 h.lst\n\
            \$ cd e\n\
            \$ ls\n\
            \584 i\n\
            \$ cd ..\n\
            \$ cd ..\n\
            \$ cd d\n\
            \$ ls\n\
            \4060174 j\n\
            \8033020 d.log\n\
            \5626152 d.ext\n\
            \7214296 k"

getInput :: IO [String]
getInput = lines <$> readFile "./input.txt"

data CLI = CDB | CD String | LS | D String | F Integer String
   deriving Show

num :: Parser Integer
num = do
    n <- many1 digit
    return (read n)

parseFile :: Parser CLI
parseFile = do
    n    <- num
    _    <- string " "
    name <- many1 anyChar
    return $ F n name

parseLS :: Parser CLI
parseLS = do
    _   <- string "$ ls"
    return LS

parseCD :: Parser CLI
parseCD = do
    _   <- string "$ cd "
    dir <- many1 anyChar
    return $ CD dir

parseCDdot :: Parser CLI
parseCDdot = do
    _   <- string "$ cd .."
    return CDB

parseDIR :: Parser CLI
parseDIR = do
    _   <- string "dir "
    dir <- many1 anyChar
    return $ D dir

parser :: Parser CLI
parser = choice $ map try [parseFile, parseLS, parseCD, parseCDdot, parseDIR]

type Path = [String]

data DirOrFile = Dir Path | File Integer String
  deriving Show

type FS = M.Map Path [DirOrFile]

parseFS :: [CLI] -> Path -> State FS FS
parseFS []     _ = get
parseFS (x:xs) p = do
    fs <- get
    case x of
        (CD "/") -> do case M.lookup [] fs of
                           Nothing -> do put $ M.insert [] [] fs
                                         parseFS xs []
                           (Just _) -> parseFS xs []
        LS -> parseFS xs p
        (F size name) -> do let lst = fromJust $ M.lookup p fs
                            put $ M.insert p (File size name : lst) fs
                            parseFS xs p
        (D name) -> do let lst = fromJust $ M.lookup p fs
                       let newDir = p ++ [name]
                       put $ M.insert newDir [] $ M.insert p (Dir newDir : lst) fs
                       parseFS xs p
        (CD "..") -> do parseFS xs (init p)
        (CD name) -> do parseFS xs $ p ++ [name]

sizes :: [(Path, [DirOrFile])] -> FS -> [(Path, Integer)]
sizes lst m = map (\(p, l) -> (p, calculateSize p l m)) lst

calculateSize :: Path -> [DirOrFile] -> FS -> Integer
calculateSize _ []          _         = 0
calculateSize p ((Dir path)  : xs) m = calculateSize path (fromJust $ M.lookup path m) m + calculateSize p xs m
calculateSize p ((File size _) : xs) m = size + calculateSize p xs m

main :: IO ()
main = do
    input <- getInput
    let lst = rights $ map (parse parser "") input
    let fs = evalState (parseFS lst []) M.empty
    let ss = sizes (M.toList fs) fs

    print "Result of part one"
    let res1 = sum $ map snd (filter (\(_, s) -> s <= 100000) ss)
    print res1

    print "Result of part two"
    let total = 70000000
    let full = maximum $ map snd ss
    let res2 = minimum $ map snd (filter (\(_, s) -> total - full + s >= 30000000) ss)
    print res2