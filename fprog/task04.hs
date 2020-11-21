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
