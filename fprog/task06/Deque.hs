module Deque
( Deque(..)
, pushFront
, pushEnd
, peekFront
, peekEnd
, popFront
, popEnd
, makeDequeFromList
) where

import Data.Maybe

data Deque a = Deque [a] deriving (Show)

-- O(1)
pushFront :: a -> Deque a -> Deque a
pushFront e (Deque d) = Deque (e : d)

-- O(n) where n is length of d
pushEnd :: a -> Deque a -> Deque a
pushEnd e (Deque d) = Deque (d ++ [e])

-- O(1)
peekFront :: Deque a -> Maybe a
peekFront (Deque d)
    | null d = Nothing
    | otherwise = Just (head d)

-- O(n)
peekEnd :: Deque a -> Maybe a
peekEnd (Deque d)
    | null d = Nothing
    | otherwise = Just (last d)

-- O(1)?
popFront :: Deque a -> Deque a
popFront (Deque d)
    | null d = Deque d
    | otherwise = Deque (tail d)

-- O(n-1)
popEnd :: Deque a -> Deque a
popEnd (Deque d)
    | null d = Deque d
    | otherwise = Deque (init d)

makeDequeFromList :: [a] -> Deque a
makeDequeFromList l = Deque l
