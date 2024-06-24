----------------------------------------------------
--              Advent of Code 2023               --
--                Day 19: Aplenty                 --
--            Solution by Lorin Lange             --
----------------------------------------------------

import qualified Data.Map as Map
import Data.List.Split ( splitOn )

type Workflows = Map.Map String [Rule]

data Rule = Conditional (Rating -> Int) Comp Res
          | Final Res

data Res = A | R | Goto String
    deriving (Eq, Ord, Show)

data Comp = LTh Int | GTh Int
    deriving Show

applyWorkflows :: Workflows -> [Rating] -> [(Rating, Bool)]
applyWorkflows wfs = map (applyWorkflow wfs)

applyWorkflow :: Workflows -> Rating -> (Rating, Bool)
applyWorkflow wfs r = applyRules (wfs Map.! "in") wfs r

applyRules :: [Rule] -> Workflows -> Rating -> (Rating, Bool)
applyRules ((Conditional f (LTh i) r):rs) wfs rating | f rating < i = applyRes r wfs rating 
                                                     | otherwise    = applyRules rs wfs rating
applyRules ((Conditional f (GTh i) r):rs) wfs rating | f rating > i = applyRes r wfs rating 
                                                     | otherwise    = applyRules rs wfs rating
applyRules ((Final res):_) wfs rating = applyRes res wfs rating

applyRes :: Res -> Workflows -> Rating -> (Rating, Bool)
applyRes A _ r = (r, True)
applyRes R _ r = (r, False)
applyRes (Goto j) wfs r = applyRules (wfs Map.! j) wfs r

parseWorkflows :: [String] -> Workflows
parseWorkflows = Map.fromList . map parseWorkflow

parseWorkflow :: String -> (String, [Rule])
parseWorkflow s =
    let (idf:xs:[]) = splitOn "{" s
        istr = splitOn "," (init xs)
        rules = map parseRule istr
    in (idf, rules)

parseRule :: String -> Rule
parseRule s | ':' `elem` s = let (c:final:[]) = splitOn ":" s
                                 (f, comp) = parseHelper c
                             in Conditional f comp (parseFinal final)
            | otherwise    = Final (parseFinal s)

parseFinal :: String -> Res
parseFinal s | s == "A"  = A
             | s == "R"  = R
             | otherwise = Goto s

parseHelper :: String -> (Rating -> Int, Comp)
parseHelper (v:xs) = (selFun v, parseComp xs)

parseComp :: String -> Comp
parseComp str | '<' `elem` str = LTh (read $ tail str)
              | otherwise      = GTh (read $ tail str)

parseRatings :: [String] -> [Rating]
parseRatings = map parseRating

parseRating :: String -> Rating
parseRating str = 
    let i = tail $ init str
        l = map (drop 2) (splitOn "," i)
        (x1:x2:x3:x4:[]) = map read l 
    in Rating { x = x1, m = x2, a = x3, s = x4 }

data Rating = Rating {
    x :: Int,
    m :: Int,
    a :: Int,
    s :: Int} 
    deriving Show

selFun :: Char -> (Rating -> Int)
selFun 'x' = \r -> x r
selFun 'm' = \r -> m r
selFun 'a' = \r -> a r
selFun 's' = \r -> s r

addingAll :: [Rating] -> Int
addingAll lst = sum $ map addingUp lst
    where addingUp Rating {x, m, a, s} = x + m + a + s

main :: IO()
main = do
    inp <- lines <$> readFile "input.txt"
    let (ws:rs:[]) = splitOn [""] inp
    let workflows = parseWorkflows ws
    let ratings = parseRatings rs    
    let lst = applyWorkflows workflows ratings
    let res1 = addingAll $ map fst $ filter snd lst
    putStrLn $ "Part 1: " ++ show res1
