module Queue
    ( Queue(..)
    , emptyQueue
    , enqueue
    , dequeue
    ) where

import SymList

type Queue a = SymList a

emptyQueue :: Queue a
emptyQueue = nilSL

enqueue :: Queue a -> a -> Queue a
enqueue queue x = consSL x queue

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue ([], []) = Nothing
dequeue queue    = Just (lastSL queue, initSL queue)
