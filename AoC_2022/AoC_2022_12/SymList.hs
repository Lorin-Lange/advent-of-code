module SymList where

type SymList a = ([a], [a])

-- maintain two invariances
-- xs and ys are lists
-- (1.) null xs => null ys || single ys
-- (2.) null ys => null xs || single xs

-- Cf. Algorithm Design with Haskell 
-- by Jeremy Gibbons and Richard Bird

single :: [a] -> Bool
single [_] = True
single _   = False

fromSL :: SymList a -> [a]
fromSL (xs, ys) = xs ++ reverse ys

nilSL :: SymList a
nilSL = ([], [])

lengthSL :: SymList a -> Int
lengthSL (xs, ys) = length xs + length ys

nullSL :: SymList a -> Bool
nullSL ([], []) = True
nullSL (_, _)   = False

singleSL :: SymList a -> Bool
singleSL ([_], [])  = True
singleSL ([], [_])  = True
singleSL (_, _)     = False

snocSL :: a -> SymList a -> SymList a
snocSL x ([], ys) = (ys, [x])
snocSL x (xs, ys) = (xs, x : ys)

lastSL :: SymList a -> a
lastSL ([], []) = error "empty SymList"
lastSL (xs, []) = head xs
lastSL (_, ys)  = head ys

tailSL :: SymList a -> SymList a
tailSL ([], [])  = error "empty SymList"
tailSL ([], ys)  = nilSL
tailSL ([_], ys) = (reverse vs, us) 
    where (us, vs) = splitAt (length ys `div` 2) ys
tailSL (xs, ys)  = (tail xs, ys)

consSL :: a -> SymList a -> SymList a
consSL x (xs, []) = ([x], xs)
consSL x (xs, ys) = (x:xs, ys)

headSL :: SymList a -> a
headSL ([], []) = error "empty SymList"
headSL ([], ys) = head ys
headSL (xs, ys) = head xs

initSL :: SymList a -> SymList a
initSL ([], [])  = error "empty SymList"
initSL (xs, [])  = nilSL
initSL (xs, [_]) = (us, reverse vs)
    where (us, vs) = splitAt (length xs `div` 2) xs
initSL (xs, ys)  = (xs, tail ys)
