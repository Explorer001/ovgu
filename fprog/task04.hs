--
-- 4.1 map, foldl, foldr, filter
--
product' :: [Int] -> Int
product' lst = foldl (*) 1 lst

allOdd :: [Int] -> Bool
allOdd lst = null (filter (==0) (map (`mod` 2) lst))

xSquaredPlusThreeXPlusFive :: [(Integer, Integer)]
xSquaredPlusThreeXPlusFive =
    map (\x -> (x, x * x + 3 * x + 5)) [0 .. 150]

getByKey :: [(String, Int)] -> String -> [Int]
getByKey lst key =
    ((map (\(_, x) -> x)) . (filter (\(x, _) -> if key == x then True else False))) lst
--
-- 4.2 Underscores, Types
--
-- (1)
--
-- _ is treated in almost all cases like a letter. It can be used in an
-- identifier. _ represents something we dont care about. Thus it can
-- be used as a wildcard.
-- Standing alone: f _ = ["foo"] -> Wildcard that matches everything.
-- as Prefix: greet firstName _lastName -> dont care about last name but
--                                         still introduce a variable.
-- Example Python:
--
-- In Python _ is a legal expression.
-- 
-- In Python _ has many uses:
--
-- 1: the interpreter stores the value of the last expression in _
--      >>> 5 + 4
--      9
--      >>> _
--      9
-- 2: _ can be used to ignore values by assigning them to _
--      >>> a, _, b = (1, 2, 3)
-- 3: Use _ in loops if you do not need a loop variable
--      for _ in range(10):
--          do_something()
--
--    (but it's still usable)
--      for _ in range(10):
--          print(_)
-- 4: Use it to separate digits
--      1_000_000 == 1000000
-- 5: Private functions
--      def _foo() is private if imported
--
-- let f _ _ = undefined is valid because _ matches everything
-- let f x x = undefined is invalid because x is used twice -> conflicting definition
--
-- (2)
--
-- :t foldl ->  foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- :t foldl1 -> foldl1 :: Foldable t => (a -> a -> a) -> t a -> a
--
-- foldl1 uses the first element of the list as first argument
--  foldl (+) 0 [1,2,3] = 6
--  foldl1 (+) [1,2,3] = 6
-- same for foldr1

--
-- 4.3 Steams and higher order functions
--
ev :: [Int]
ev = filter (\x -> x `mod` 2 == 0) [0 ..]

harmonic :: (Fractional a, Enum a) => [a]
harmonic = map (1/) (tail [0 ..])

triangle :: [Int]
triangle = map (\x -> foldr1 (+) [1 .. x]) (tail [0 ..])

palin :: [Int]
palin = filter (\x -> show x == reverse (show x)) [0 ..]

--
-- 4.4 Advanced folding and filtering
--
dfold :: (b -> c -> c) -> c -> (a -> b -> b) -> b -> [[a]] -> c
dfold op1 val1 op2 val2 lst = foldr op1 val1 (map (foldr op2 val2) lst)

nfilter :: [(a -> Bool)] -> [a] -> [a]
nfilter conditions lst = filter (\x -> foldr (&&) True (map (\y -> y x) conditions)) lst

--
-- 4.5 Function family and stream of streams
--
f_all :: [[Int]]
f_all = map (\y -> map (\x -> x * x + x + y) [0 ..]) [0 ..]

ttake :: Int -> Int -> [[a]] -> [a]
ttake ntake add stream = take ntake (stream !! add)
