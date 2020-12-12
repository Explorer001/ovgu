import Test.QuickCheck

--
-- Assumin ° <==> .
--
-- f1 = reverse
-- f2 = ++ 
-- f3 = : 
-- f4 = head
-- f5 = tail
-- f6 = length
--

--
-- 1. a == f1 . f2 a
--
prop1 :: [Int] -> Bool
prop1 a = a == (reverse . reverse) a

--
-- 2. f2(f1 b)(f1 a) == f1(f2 a b)
--
prop2 :: [Int] -> [Int] -> Bool
prop2 a b = (reverse b) ++ (reverse a) == reverse (a ++ b)

--
-- 3. not (null a) ==> (a == f3(f4 a)(f5 a))
--
prop3 :: [Int] -> Property
prop3 a = not (null a) ==> a == (head a) : (tail a)

--
-- 4. f6 a + f6 b = f6 (f2 a b)
--
prop4 :: [Int] -> [Int] -> Bool
prop4 a b = length a + length b == length (a ++ b)

--
-- 5. not (null a) ==> (f2 (f1 . f5 a)([f4 a]) == f1 a)
--
prop5 :: [Int] -> Property
prop5 a = not (null a) ==> (reverse . tail) a ++ ([head  a]) == reverse a

main = do
    quickCheck prop1
    quickCheck prop2
    quickCheck prop3
    quickCheck prop4
    quickCheck prop5
