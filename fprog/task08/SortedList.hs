import Data.List
import Test.QuickCheck

data SL a = SL [a] deriving (Show)

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
prop_min :: SL Int -> Bool
prop_min (SL l)
    | null l = True
    | otherwise = head l == foldr1 min l

prop_max :: SL Int -> Bool
prop_max (SL l)
    | null l = True
    | otherwise = head l == foldr1 max l

--
-- isSorted(x:s) = True
--
-- Nope, simple append to list can break invariant
prop_sorted :: Int -> SL Int -> Bool
prop_sorted e (SL l) = isSorted (e:l) == True

--
-- (x = head(s)) => length (s) = length (insert x s)
--
-- No. List can contain element multiple times
prop_insert :: Int -> SL Int -> Property
prop_insert e (SL l) = not (null l) && e == head l ==> length l == length (insert e l) 

--
-- (x <  head(s)) => (isSorted(x:s)) = True
--
-- Again: depends on sort function: ascending or descending
prop_append :: Int -> SL Int -> Property
prop_append e (SL l) = not (null l) && e < head l ==> isSorted (e:l) == True

--
-- isSorted(x) && isSorted(y) => isSorted(x++y)
--
-- No... Just no
prop_sorted1 :: SL Int -> SL Int -> Property
prop_sorted1 (SL l1) (SL l2) = isSorted(l1) && isSorted(l2) ==> isSorted(l1++l2) 

instance (Arbitrary a, Ord a) => Arbitrary (SL a) where
    arbitrary = do
        x <- arbitrary
        return $ SL (sort [y | y <-x])

main = do
    quickCheck prop_min
    quickCheck prop_max
    quickCheck prop_sorted
    quickCheck prop_insert
    quickCheck prop_append
    quickCheck prop_sorted1
