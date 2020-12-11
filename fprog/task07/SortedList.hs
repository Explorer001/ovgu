import Data.List
import Test.QuickCheck

type SL a = [a]

sfromList :: Ord a => [a] -> SL a
sfromList l = sort l

--
-- Quick Check properties
--

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted(y:xs)

--
-- minimum = head(s)
-- maximum = head(s)
--
-- depends on sort function -> only one can be true depending on order
-- furthermore this does not hold for empty list
prop_min :: [Int] -> Bool
prop_min l
    | null l = True
    | otherwise = head (sfromList l) == foldr1 min l

prop_max :: [Int] -> Bool
prop_max l
    | null l = True
    | otherwise = head (sfromList l) == foldr1 max l

--
-- isSorted(x:s) = True
--
-- Nope, simple append to list can break invariant
prop_sorted :: Int -> [Int] -> Bool
prop_sorted e l = isSorted (e:(sfromList l)) == True

--
-- (x = head(s)) => length (s) = length (insert x s)
--
-- No. List can contain element multiple times
prop_insert :: Int -> [Int] -> Property
prop_insert e l = not (null l) && e == head (sfromList l) ==> length l == length (insert e (sfromList l)) 

--
-- (x <  head(s)) => (isSorted(x:s)) = True
--
-- Again: depends on sort function: ascending or descending
prop_append :: Int -> [Int] -> Property
prop_append e l = not (null l) && e < head (sfromList l) ==> isSorted (e:(sfromList l)) == True

--
-- isSorted(x) && isSorted(y) => isSorted(x++y)
--
-- No... Just no
prop_sorted1 :: [Int] -> [Int] -> Property
prop_sorted1 l1 l2 = let s1 = sort l1
                         s2 = sort l2 in
                            isSorted(s1) && isSorted(s2) ==> isSorted(s1++s2) 

main = do
    quickCheck prop_min
    quickCheck prop_max
    quickCheck prop_sorted
    quickCheck prop_insert
    quickCheck prop_append
    quickCheck prop_sorted1
