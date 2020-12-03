module SortedList
( slempty
, slappend
, slhead
, sltail
, slnull
, sllength
) where

data SortedList a = SortedList [a] deriving (Show)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (p:xs) = (qsort lesser) ++ [p] ++ (qsort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs

slempty :: SortedList a
slempty = SortedList []

slappend ::Ord a => a -> SortedList a -> SortedList a
slappend e (SortedList l) = SortedList (qsort (e : l))

slhead :: SortedList a -> a
slhead (SortedList l) = head l

sltail :: SortedList a -> SortedList a
sltail (SortedList l) = SortedList (tail l)

sllength :: SortedList a -> Int
sllength (SortedList l) = length l

slnull :: SortedList a -> Bool
slnull (SortedList l) = null l
