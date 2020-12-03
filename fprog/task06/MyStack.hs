module MyStack
( newStack
, pop
, push
, peek
) where

import Deque
import Data.Maybe

data MyStack a = MyStack (Deque a) deriving (Show)

-- O(1)
newStack :: MyStack a
newStack = MyStack (Deque [])

-- O(1)?
pop :: MyStack a -> MyStack a
pop (MyStack s) = MyStack (popFront s)

-- O(1)
push :: a -> MyStack a -> MyStack a
push e (MyStack s) = MyStack (pushFront e s)

-- O(1)
peek :: MyStack a -> Maybe a
peek (MyStack s) = peekFront s
