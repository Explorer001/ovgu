--
-- 5.1 Comprehensible Streams
--
harmonic :: (Fractional a, Enum a) => [a]
harmonic = map (1/) (tail [0 ..])

palin :: [Int]
palin = filter (\x -> show x == reverse (show x)) [0 ..]

partsum :: Num a => [a] -> [a]
partsum xs = [x | x <- scanl (+) 0 xs]

--
-- 5.2 The Functor class and its limits
--
-- data Bool_ = True_ | False_
-- + cannot be implemented
-- + Functor expects '* -> *' but Bool_ has kind '*'
--
-- data List a = List a (List a) | Empty
-- + can be implemented
-- 
-- data Either_ a b = Right_ a | Left_ b
-- + cannot be implemented
-- + Functor expects '* -> *' but Eihter_ has kind '* -> * -> *'
-- + can be fixed by 'data Either_ a' but why use this at all then? 
--
-- data Maybe_ a = Nothing_ | Just_ a
-- + can be implemented
--
-- data Pair a b = Pair a b
-- + cannot be implemented
-- + Functor expects '* -> *' but Eihter_ has kind '* -> * -> *'
-- + Fix: 'data Pair a = Pair a a'
--
-- data LList a = LList [a] (a,a) a
-- + can be implemented

--
-- 5.3 The Functor class and its limits continued
--
data List a = List a (List a) | Empty deriving (Show)

instance Functor List where
    fmap f (List val nxt) = List (f val) (fmap f nxt)
    fmap f Empty = Empty

data Maybe_ a = Nothing_ | Just_ a deriving (Show)

instance Functor Maybe_ where
    fmap f (Just_ val) = Just_ (f val)
    fmap f Nothing_ = Nothing_

data Pair a = Pair a a deriving (Show)

instance Functor Pair where
    fmap f (Pair v1 v2) = Pair (f v1) (f v2)

data LList a = LList [a] (a,a) a deriving (Show)

instance Functor LList where
    fmap f (LList lst (t1, t2) val) = LList (fmap f lst) (f t1, f t2) (f val)

--
-- 5.4 Folding
--
data Seq a = End | Seq a (Seq a) | Lseq [a] (Seq a) | Pseq (a,a) (Seq a) deriving (Show)

instance Functor Seq where
    fmap f (Seq v s) = Seq (f v) (fmap f s)
    fmap f (Lseq v s) = Lseq (fmap f v) (fmap f s)
    fmap f (Pseq (v1, v2) s) = Pseq (f v1, f v2) (fmap f s)
    fmap f End = End

instance Foldable Seq where
    foldr f n (Seq v s) = f v (foldr f n s)
    foldr f n (Lseq v s) = foldr f (foldr f n s) v
    foldr f n (Pseq (v1, v2) s) = f v2 (f v1 (foldr f n s))
    foldr f n End = n

--
-- 5.6 Not enough Folding and Mapping: Functor^3 and Foldable^3
--
data Triple a b c = Triple a b c deriving (Show)

class Trifunctor p where
    trimap :: (a -> b) -> (c -> d) -> (e -> f) -> p a c e -> p b d f

class Trifoldable p where
    trifoldr :: (a -> c -> c) -> (b -> c -> c) -> (d -> c -> c) -> c -> p a b d -> c

instance Trifunctor Triple where
    trimap f1 f2 f3 (Triple v1 v2 v3) = Triple (f1 v1) (f2 v2) (f3 v3)

instance Trifoldable Triple where
    trifoldr f1 f2 f3 n (Triple v1 v2 v3) = f1 v1 (f2 v2 (f3 v3 n))
