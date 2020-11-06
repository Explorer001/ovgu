import Data.Maybe
--
-- Task 2.1 Basic Casino
--

-- all possible jeton types
data Jeton = Red | Green | Blue | Silver | Gold

-- jeton collection
data Jetons = JC Jeton Jetons | Empty

-- gets corresponding money value
value :: Jeton -> Int
value j = case j of
    Red -> 1
    Green -> 5
    Blue -> 10
    Silver -> 50
    Gold -> 100

-- count number of jetons in collection
count :: Jetons -> Int
count j = case j of
    Empty -> 0
    JC j jc -> 1 + count(jc)

-- converts jetons in money
payoff :: Jetons -> Int
payoff j = case j of
    Empty -> 0
    JC j jc -> value j + payoff(jc)

-- buy jetons for money
buy :: Int -> Jetons
buy m
    | m <= 0 = Empty
    | m >= value Gold  = JC Gold (buy (m - (value Gold)))
    | m >= value Silver = JC Silver (buy (m - (value Silver)))
    | m >= value Blue = JC Blue (buy (m - (value Blue)))
    | m >= value Green = JC Green (buy (m - (value Green)))
    | m >= value Red = JC Red (buy (m - (value Red)))
    | otherwise = Empty

--
-- Task 2.2 Text alignments
--
-- TODO: implement

-- alignment types
data Format = Flushleft | Flushright

-- gets first position of char in string
getFirst :: Char -> String -> Int
getFirst c s
    | null s = -1
    | c == head s = 0
    | otherwise =
        let r = getFirst c (tail s)
        in if r < 0 then -1 else 1 + r

splitOn :: Char -> String -> [String]
splitOn c s =
    let f = getFirst c s
    in if f < 0 then if null s then [] else [s]
       else [take f s] ++ splitOn c (drop (f + 1) s)

--
-- Task 2.3 Safe calculations
--
-- Maybe Double catches illegal cases. division by 0
-- is forbidden. thus safe div is undefined for x/0 and returns Nothing.
-- in this case setting the function parameters to Maybe is unnecessary
--

-- safe division
safeDiv :: Maybe Double -> Maybe Double -> Maybe Double
safeDiv a b
    | isNothing a = Nothing -- a undefined
    | isNothing b = Nothing -- b undefined
    | fromJust b == 0 = Nothing -- division by 0 prohibided
    | otherwise = Just (fromJust a / fromJust b)

-- safe square root
safeSqrt :: Maybe Double -> Maybe Double
safeSqrt a
    | isNothing a = Nothing -- param is undefined
    | fromJust a < 0 = Nothing -- only defined for natural numbers including 0
    | otherwise = Just (sqrt (fromJust a))

--
-- Task 2.4
--
-- There is a simpler Enumeration in Haskell: the Unit Type.
-- it is defined by: data () = () with constructor () :: ().
-- it is a enumeration with exactly 1 value: ().
-- it can be used to indicate that a function does not return something
-- interesting.
--

--
-- Task 2.5 Hiding Values
--
data ListInt = ListInt [Int] Int deriving (Show)

-- ListInt creator
create :: [Int] -> ListInt
create l = ListInt l (sum l)

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
-- 2.6 Mixed Lists
--
data Node = NodeString String | NodeInt Int | NodeBool Bool deriving (Show)
data ML = ML [Node] deriving (Show)

-- extract ints
extractInt :: ML -> [Int]
extractInt (ML l)
    | null l = []
    | otherwise = let h = head l in
                  case h of
                    NodeInt i -> i : extractInt (ML (tail l))
                    _ -> extractInt (ML (tail l))

-- extract strings
extractString :: ML -> [String]
extractString (ML l)
    | null l = []
    | otherwise = let h = head l in
                   case h of
                    NodeString s -> s : extractString (ML (tail l))
                    _ -> extractString (ML (tail l))

-- extract bools
extractBool :: ML -> [Bool]
extractBool (ML l)
    | null l = []
    | otherwise = let h = head l in
                   case h of
                    NodeBool s -> s : extractBool (ML (tail l))
                    _ -> extractBool (ML (tail l))

-- concat two mixed lists
mlconcat :: ML -> ML -> ML
mlconcat (ML l1) (ML l2) = ML (l1 ++ l2)

-- prepend list
prepend :: ML -> ML -> ML
prepend (ML l1) (ML l2)
    | length l1 == 1 = ML (l1 ++ l2)
    | otherwise = error "List too short or too long!"

-- counting functions
countInt :: ML -> Int
countInt (ML l)
    | null l = 0
    | otherwise = case head l of 
                    NodeInt _ -> 1 + countInt (ML (tail l))
                    _ -> countInt (ML (tail l))

countString :: ML -> Int
countString (ML l)
    | null l = 0
    | otherwise = case head l of
                    NodeString _ -> 1 + countString (ML (tail l))
                    _ -> countString (ML (tail l))

countBool :: ML -> Int
countBool (ML l)
    | null l = 0
    | otherwise = case head l of
                    NodeBool _ -> 1 + countBool (ML (tail l))
                    _ -> countBool (ML (tail l))

-- count components
countComponents :: ML -> String
countComponents l = "Strings: " ++ show (countString l) ++
                    ", Integers: " ++ show (countInt l) ++
                    ", Bools: " ++ show (countBool l) 
