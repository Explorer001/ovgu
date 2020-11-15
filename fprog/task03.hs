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
-- Task 3.3 components and products
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

-- checks if item is producible
isProducible :: (Eq a, Eq b) => Product b a -> Storage a -> Bool
isProducible prod sto = isProducible' (getProdList prod) sto where
    isProducible' :: Eq a => [(a, Int)] -> Storage a -> Bool
    isProducible' lst sto
        | null lst = True
        | otherwise = let cont = contains sto (fst (head lst)) in
            if isNothing cont then False
            else if fromJust cont < fromInteger (toInteger (snd (head lst))) then False
            else isProducible' (tail lst) sto

-- produces given component if possible
produce :: (Eq a, Eq b) => Product b a -> Storage a -> Storage a
produce prod sto
    | isProducible prod sto == False = sto
    | otherwise = produce' (getProdList prod) sto where
        produce' :: Eq a => [(a, Int)] -> Storage a -> Storage a
        produce' lst sto
            | null lst = sto
            | otherwise = produce' (tail lst) (remove sto (fst (head lst)) (fromInteger (toInteger (snd (head lst)))))

--
-- Task 3.4 Polymorphism Quirks 
--
-- 1.
-- 
-- "" == []
--     :t "" = "" :: [Char], :t [] = [] :: [a]
--     so [] is a generic list and "" a char list -> evaluates to true
--
-- tail [1] == ""
--     :t tail [1] = tail [1] :: Num a => [a]
--     constraint that generic type a has to be num -> not statisfied by char
--
-- tail [1] == []
--     generic type a statisfies Num a
--
-- 2.
--
-- read "10" == 0 evaluates to true. this is because the _ == 10 operator
-- expects to compare the first argument with an integer. read "10" returns
-- this integer. if read "10" is executed in ghci, ghci expects a string to
-- be able to execute show but read "10" returns a int. this can be fixed
-- by reading a string with 'read "\"10\"" :: String'

--
-- Task 3.5 Sets
--
data Set a = Set [a] deriving (Show)

-- creates unique set from list
createSet :: (Eq a, Ord a) => [a] -> Set a
createSet lst
    | null lst = Set lst
    | otherwise = Set (createSet' (sort lst)) where
        createSet' :: (Eq a, Ord a) => [a] -> [a]
        createSet' lst
            | null lst = []
            | otherwise = let n = createSet' (tail lst) in
                if null n then head lst : n 
                else if head n /= head lst then head lst : n
                else n

-- checks if set conatain value
setContains :: Eq a => [a] -> a -> Bool
setContains lst value
    | null lst = False
    | head lst == value = True
    | otherwise = setContains (tail lst) value

-- intersection of sets
intersection :: Eq a => Set a -> Set a -> Set a
intersection (Set l1) (Set l2) = Set (intersection' l1 l2) where
        intersection' :: Eq a => [a] -> [a] -> [a]
        intersection' l1 l2
            | null l1 = []
            | setContains l2 (head l1) = head l1 : intersection' (tail l1) l2
            | otherwise = intersection' (tail l1) l2

-- complement of sets
setMinus :: Eq a => Set a -> Set a -> Set a
setMinus (Set l1) (Set l2) = Set (setMinus' l1 l2) where
    setMinus' :: Eq a => [a] -> [a] -> [a]
    setMinus' l1 l2
        | null l1 = []
        | setContains l2 (head l1) = setMinus' (tail l1) l2
        | otherwise = head l1 : setMinus' (tail l1) l2

-- union of sets
union :: Eq a => Set a -> Set a -> Set a
union s1 s2 = let s3 = intersection s1 s2 in concat' (setMinus s1 s3) s2 where
    concat' :: Eq a => Set a -> Set a -> Set a
    concat' (Set l1) (Set l2) = Set (l1 ++ l2)

-- checks if given set is empty
setEmpty :: Eq a => Set a -> Bool
setEmpty (Set l1) = null l1

instance Eq a => Eq (Set a) where
    s1 == s2 = if setEmpty (setMinus s1 s2) && setEmpty (setMinus s2 s1) then True
               else False

instance Ord a => Ord (Set a) where
    s1 `compare` s2 =
        if s1 == s2 then EQ
        else if intersection s1 s2 == s1 then LT
        else GT
