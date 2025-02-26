----------------------------------------------------
--              Advent of Code 2022               --
--        Day 12: Hill Climbing Algorithm         --
--            Solution by Lorin Lange             --
----------------------------------------------------

module Queue where

import qualified Data.Sequence as Seq

type Queue a = Seq.Seq a

emptyQueue :: Queue a
emptyQueue = Seq.empty

enqueue :: Queue a -> a -> Queue a
enqueue queue x = x Seq.<| queue

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue queue = 
    case Seq.viewr queue of
        Seq.EmptyR   -> Nothing
        (s Seq.:> x) -> Just (x, s)

peek :: Queue a -> Maybe a
peek queue =
    case Seq.viewr queue of
        Seq.EmptyR   -> Nothing
        (s Seq.:> x) -> Just x

null :: Queue a -> Bool
null = Seq.null

notEmpty :: Queue a -> Bool
notEmpty = not . Seq.null

enqueueFromList :: Queue a -> [a] -> Queue a
enqueueFromList = foldl enqueue
