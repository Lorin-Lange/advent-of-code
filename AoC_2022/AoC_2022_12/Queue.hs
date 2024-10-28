----------------------------------------------------
--              Advent of Code 2022               --
--        Day 12: Hill Climbing Algorithm         --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Queue where

import SymList

type Queue a = SymList a

emptyQueue :: Queue a
emptyQueue = nilSL

enqueue :: Queue a -> a -> Queue a
enqueue queue x = consSL x queue

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue ([], []) = Nothing
dequeue queue    = Just (lastSL queue, initSL queue)

peek :: Queue a -> Maybe a
peek ([], []) = Nothing
peek queue    = Just $ lastSL queue

notEmpty :: Queue a -> Bool
notEmpty = not . nullSL

enqueueFromList :: Queue a -> [a] -> Queue a
enqueueFromList = foldl enqueue
