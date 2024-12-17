----------------------------------------------------
--              Advent of Code 2024               --
--         Day 17: Chronospatial Computer         --
--            Solution by Lorin Lange             --
----------------------------------------------------

{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE LambdaCase            #-}

module Main where

import Data.Bits ( shiftR, xor , (.&.) )
import Data.List.Split ( splitOn )

data Registers = R { a, b, c :: Int }
    deriving Show

parse :: [String] -> (Registers, [Int])
parse lst = (R { a, b, c }, instr)
    where a = read $ splitOn ": " (head lst) !! 1
          b = read $ splitOn ": " (lst !! 1) !! 1
          c = read $ splitOn ": " (lst !! 2) !! 1
          instr = map read . splitOn "," . drop 9 $ lst !! 4

run :: (Registers, [Int]) -> String
run (rs, instr) = tail . init $ show out where (_, _, out) = runH (rs, 0, []) instr

runH :: (Registers, Int, [Int]) -> [Int] -> (Registers, Int, [Int])
runH t@(rs, ip, out) instr = if ip >= length instr then t
    else runH (execute (instr !! ip) ip (instr !! (ip + 1)) rs out) instr

execute :: Int -> Int -> Int -> Registers -> [Int] -> (Registers, Int, [Int])
execute 0 ip op rs out = (rs { a = rs.a `shiftR` comboOp op rs }, ip + 2, out)
execute 1 ip op rs out = (rs { b = rs.b `xor` op }, ip + 2, out)
execute 2 ip op rs out = (rs { b = comboOp op rs .&. 7 }, ip + 2, out)
execute 3 ip op rs out = (rs, if rs.a == 0 then ip + 2 else op, out)
execute 4 ip op rs out = (rs { b = rs.b `xor` rs.c }, ip + 2, out)
execute 5 ip op rs out = (rs, ip + 2, out ++ [comboOp op rs .&. 7])
execute 6 ip op rs out = (rs { b = rs.a `shiftR` comboOp op rs }, ip + 2, out)
execute 7 ip op rs out = (rs { c = rs.a `shiftR` comboOp op rs }, ip + 2, out)

comboOp :: Int -> Registers -> Int
comboOp = \cases 4 rs -> rs.a; 5 rs -> rs.b; 6 rs -> rs.c; o _ -> o

main :: IO()
main = do t@(rs, cmds) <- parse . lines <$> readFile "input.txt"
          putStrLn $ "Part 1: " ++ run t
          putStrLn $ "Part 2: " ++ show ("TODO")
