module Deque
( pushFront
, pushEnd
, peekFront
, peekEnd
, popFront
, popEnd
, makeDequeFromList
) where

import Data.Maybe

data Deque a = Deque [a] deriving (Show)

pushFront :: a -> Deque a -> Deque a
pushFront e (Deque d) = Deque (e : d)

pushEnd :: a -> Deque a -> Deque a
pushEnd e (Deque d) = Deque (d ++ [e])

peekFront :: Deque a -> Maybe a
peekFront (Deque d)
    | null d = Nothing
    | otherwise = Just (head d)

peekEnd :: Deque a -> Maybe a
peekEnd (Deque d)
    | null d = Nothing
    | otherwise = Just (last d)

popFront :: Deque a -> Deque a
popFront (Deque d)
    | null d = Deque d
    | otherwise = Deque (tail d)

popEnd :: Deque a -> Deque a
popEnd (Deque d)
    | null d = Deque d
    | otherwise = Deque (init d)

makeDequeFromList :: [a] -> Deque a
makeDequeFromList l = Deque l
