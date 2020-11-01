-- sum int with if else syntax
sum_int :: Int -> Int
sum_int n = if n <= 0 then 0
            else if n == 1 then 1
                 else n + sum_int (n - 1)

-- sum int with guard syntax
sum_int_alt :: Int -> Int
sum_int_alt n
    | n <= 0 = 0
    | n == 1 = 1
    | otherwise = n + sum_int_alt (n - 1)

-- digit sum with if else syntax
digit_sum :: Integer -> Integer
digit_sum n = if n <= 0 then 0
              else (mod n 10) + (digit_sum (div n 10))

-- digit sum with guard syntax
digit_sum_alt :: Integer -> Integer
digit_sum_alt n
    | n <= 0 = 0
    | otherwise = (mod n 10) + (digit_sum_alt (div n 10))

-- function for negative digit sum
digit_sum_neg :: Integer -> Integer
digit_sum_neg n
    | n < 0 = (-digit_sum_alt (n * (-1)))
    | otherwise = digit_sum_alt (n)

-- prefix implementation with if else syntax
isPrefixOf :: String -> String -> Bool
isPrefixOf p s = if null s then False
                 else if null p then True
                      else if head p /= head s then False
                           else isPrefixOf (tail p) (tail s)

-- prefix implementation with guard syntax
isPrefixOfAlt :: String -> String -> Bool
isPrefixOfAlt p s
    | null s = False
    | null p = True
    | head p /= head s = False
    | otherwise = isPrefixOfAlt (tail p) (tail s)

-- 1.4 b: try/catch, if/else
-- 1.5 pythons range is a lazy iterator
--     mixing paradigms is not a good idea -> mixed code

-- completing prefixes
asPrefixOf :: String -> String -> String
asPrefixOf p s
    | null p && null s = ""
    | null p = s
    | null s = p
    | head p /= head s = head p : asPrefixOf (tail p) s
    | otherwise = head s : asPrefixOf (tail p) (tail s)
