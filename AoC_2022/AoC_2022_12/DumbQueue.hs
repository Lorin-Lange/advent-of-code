----------------------------------------------------
--              Advent of Code 2022               --
--        Day 12: Hill Climbing Algorithm         --
--            Solution by Lorin Lange             --
----------------------------------------------------

module DumbQueue where

type Queue a = [a]

emptyQueue :: Queue a
emptyQueue = []

enqueue :: Queue a -> a -> Queue a
enqueue queue x = x : queue

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue []    = Nothing
dequeue queue = Just (last queue, init queue)

peek :: Queue a -> Maybe a
peek []    = Nothing
peek queue = Just $ last queue

notEmpty :: Queue a -> Bool
notEmpty = not . null

enqueueFromList :: Queue a -> [a] -> Queue a
enqueueFromList = foldl enqueue
