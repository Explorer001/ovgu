--
-- Task 3.1 Splitting Lists
--
splitL :: Int -> [a] -> [[a]]
splitL index list 
    | index >= length list = [list]
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
        x : mSplitL (map (subtract (head ints + 1)) (tail ints)) (head xs)
