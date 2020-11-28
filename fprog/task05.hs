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
