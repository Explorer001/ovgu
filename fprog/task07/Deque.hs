import Data.Maybe
import Test.QuickCheck

type Deque a = [a]

-- O(1)
pushFront :: a -> Deque a -> Deque a
pushFront e d = e : d

-- O(n) where n is length of d
pushEnd :: a -> Deque a -> Deque a
pushEnd e d = d ++ [e]

-- O(1)
peekFront :: Deque a -> Maybe a
peekFront d
    | null d = Nothing
    | otherwise = Just (head d)

-- O(n)
peekEnd :: Deque a -> Maybe a
peekEnd d
    | null d = Nothing
    | otherwise = Just (last d)

-- O(1)?
popFront :: Deque a -> Deque a
popFront d
    | null d = d
    | otherwise = tail d

-- O(n-1)
popEnd :: Deque a -> Deque a
popEnd d
    | null d = d
    | otherwise = init d

makeDequeFromList :: [a] -> Deque a
makeDequeFromList l = l

--
-- QuickCheck properties
--

-- popFront on empty deque should be empty
-- No entry in Queue -> only can be empty
prop_empty_front :: Bool
prop_empty_front = null (popFront (makeDequeFromList [])) == True

-- popEnd on empty deque should be empty
-- No entry in Queue -> only can be empty
prop_empty_end :: Bool
prop_empty_end = null (popFront (makeDequeFromList [])) == True

-- peekFront on empty deque is undefined
-- No entry in Queue -> read has to be undefined
prop_empty_peek_front :: Bool
prop_empty_peek_front = isNothing (peekFront (makeDequeFromList [])) == True

-- peekFront on empty deque is undefined
-- No entry in Queue -> read has to be undefined
prop_empty_peek_end :: Bool
prop_empty_peek_end = isNothing (peekEnd (makeDequeFromList [])) == True

-- peekFront on non empty queue should be head of initial list
prop_peek_front :: [Int] -> Bool
prop_peek_front lst
    | null lst = True
    | otherwise = peekFront (makeDequeFromList lst) == Just (head lst)

-- peekEnd on non empty queue should be last element of initial list
prop_peek_end :: [Int] -> Bool
prop_peek_end lst
    | null lst = True
    | otherwise = peekEnd (makeDequeFromList lst) == Just (last lst)

main = do
    quickCheck prop_empty_front
    quickCheck prop_empty_end
    quickCheck prop_empty_peek_front
    quickCheck prop_empty_peek_end
    quickCheck prop_peek_front
    quickCheck prop_peek_end
