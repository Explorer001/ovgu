import Numeric.Natural
import Data.Maybe
--
-- Task 3.1 Splitting Lists
--
splitL :: Int -> [a] -> [[a]]
splitL index list 
    | index - 1 >= length list = [list]
    | otherwise = [getFirstN (index + 1) list, drop (index + 1) list] where
        getFirstN :: Int -> [a] -> [a]
        getFirstN n list
            | n >= length list = list
            | n == 0 = []
            | null list = []
            | otherwise = (head list) : getFirstN (n - 1) (tail list)

mSplitL :: [Int] -> [a] -> [[a]]
mSplitL ints list
    | null list = [[]]
    | null ints = [list]
    | otherwise = let x:xs = splitL (head ints) list in
        if null xs then x : [[]] else
        x : mSplitL (map (subtract (head ints + 1)) (tail ints)) (head xs)

---
-- Task 3.2 Sorting
--
merge :: Ord a => [a] -> [a] -> [a]
merge l1 l2
    | (length l1 /= 0) && (length l2 /= 0) = if head l1 <= head l2 then
                                                head l1 : merge (tail l1) l2
                                             else
                                                head l2 : merge l1 (tail l2)
    | (length l1 /= 0) = head l1 : merge (tail l1) l2
    | (length l2 /= 0) = head l2 : merge l1 (tail l2)
    | otherwise = []


sort :: Ord a => [a] -> [a]
sort list
    | length list <= 1 = list
    | otherwise = let splt = splitAt (div (length list) 2) list in
                    merge (sort (fst splt)) (sort (snd splt)) 

data ListInt = ListInt [Int] Int deriving (Show)

-- ListInt creator
create :: [Int] -> ListInt
create l = ListInt l (sum l)

-- eq
instance Eq ListInt where
    (ListInt l1 n1) == (ListInt l2 n2) =
        if n1 == n2 then True else False

--ord
instance Ord ListInt where
    (ListInt l1 n1) `compare` (ListInt l2 n2)
        | length l1 < length l2 = LT
        | length l1 > length l2 = GT
        | length l1 == length l2 =
            if n1 < n2 then LT else if n1 > n2 then GT else EQ
        | otherwise = EQ

-- add item to list
add :: Int -> ListInt -> ListInt
add i (ListInt l s) = ListInt (l ++ [i]) (s + i)

-- concat two ListInts
liconcat :: ListInt -> ListInt -> ListInt
liconcat (ListInt l1 s1) (ListInt l2 s2) = ListInt (l1 ++ l2) (s1 + s2)

-- extract list from list int
getList :: ListInt -> [Int]
getList (ListInt l _) = l

-- get Sum from list int
getSum :: ListInt -> Int
getSum (ListInt _ s) = s

--
-- Task 3.3
--
data Component a = Component a Natural deriving (Show)
type Storage a = [Component a]
data Product a b = Product a [(b,Int)] deriving (Show)

--getter for product and component
getCompID :: Component a -> a
getCompID (Component id _) = id

getCompCount :: Component a -> Natural
getCompCount (Component _ count) = count

getProdID :: Product a b -> a
getProdID (Product id _) = id

getProdList :: Product a b -> [(b,Int)]
getProdList (Product  _ lst) = lst


-- maybe returns the amount of components in storage
contains :: Eq a => Storage a -> a -> Maybe Natural
contains sto cmp
    | null sto = Nothing
    | getCompID (head sto) == cmp = Just (getCompCount (head sto))
    | otherwise = contains (tail sto) cmp

-- creates or updates storage entry
store :: Eq a => Storage a -> a -> Natural -> Storage a
store sto dsc n
    | isNothing (contains sto dsc) = Component dsc n : sto
    | otherwise = store' sto dsc n where
        store' :: Eq a => Storage a -> a -> Natural -> Storage a
        store' sto dsc n
            | null sto = []
            | getCompID (head sto) == dsc = Component dsc ((fromJust (contains sto dsc)) + n) 
                                                : store' (tail sto) dsc n
            | otherwise = head sto : store' (tail sto) dsc n

-- removes given item from storage
remove :: Eq a => Storage a -> a -> Natural -> Storage a
remove sto dsc n
    | null sto = []
    | getCompID (head sto) == dsc = let cc = getCompCount (head sto) in
        if cc - n <= 0 then remove (tail sto) dsc n
        else Component dsc (cc - n) : remove (tail sto) dsc n
    | otherwise = head sto : remove (tail sto) dsc n
