module MyQueue
( newQueue
, enqueue
, dequeue
, peek
) where

import Deque
import Data.Maybe

data MyQueue a = MyQueue (Deque a) deriving (Show)

-- O(1)
newQueue :: MyQueue a
newQueue = MyQueue (Deque [])

-- O(n)
enqueue :: a -> MyQueue a -> MyQueue a
enqueue e (MyQueue q) = MyQueue (pushEnd e q)

-- O(1)?
dequeue :: MyQueue a -> MyQueue a
dequeue (MyQueue q) = MyQueue (popFront q)

-- O(1)
peek :: MyQueue a -> Maybe a
peek (MyQueue q) = peekFront q
