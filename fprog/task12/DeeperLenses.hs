import Lens

data Date = Date {day :: Int, month :: Int, year :: Int}
data Person = Person {firstName :: String, lastName :: String, birthDate :: Date}
data Flat = Flat {owner :: Person, size :: (Int, Int), lastrenovation :: Date}
data Floor = Floor {floornumber :: Int, leftFlat :: Flat, rightFlat :: Flat}
data SmallHouse = SmallHouse {groundFloor :: Floor, firstFloor :: Floor}

x = SmallHouse (Floor 0 (Flat (Person "john" "doe" (Date 1 2 3333)) (20,20) (Date 0 0 2340)) (Flat (Person "jane" "doe" (Date 14 23 2224)) (20,20) (Date 0 0 2340))) (Floor 1 (Flat (Person "jack" "doe" (Date 1 24 1425)) (20,20) (Date 0 0 2240)) (Flat (Person "judith" "doe" (Date 12 2 1102)) (20,20) (Date 0 0 2340)))

lYear :: Lens' Date Int
lYear fn (Date d m y) = fmap (\y' -> Date d m y') (fn y)

lPersBirthDate :: Lens' Person Date
lPersBirthDate fn (Person f l b) = fmap (\b' -> Person f l b') (fn b)

lLastName :: Lens' Person String
lLastName fn (Person f l b) = fmap (\l' -> Person f l' b) (fn l)

lFlatOwner :: Lens' Flat Person
lFlatOwner fn (Flat o s l) = fmap (\o' -> Flat o' s l) (fn o)

lFlatSize :: Lens' Flat (Int, Int)
lFlatSize fn (Flat o s l) = fmap (\s' -> Flat o s' l) (fn s)

lFirstFloor :: Lens' SmallHouse Floor
lFirstFloor fn (SmallHouse g f) = fmap (\f' -> SmallHouse g f') (fn f)

lGroundFloor :: Lens' SmallHouse Floor
lGroundFloor fn (SmallHouse g f) = fmap (\g' -> SmallHouse g' f) (fn g)

lLeftFlat :: Lens' Floor Flat
lLeftFlat fn (Floor f l r) = fmap (\l' -> Floor f l' r) (fn l)

lRightFlat :: Lens' Floor Flat
lRightFlat fn (Floor f l r) = fmap (\r' -> Floor f l r') (fn r)

lExaggeratedOne :: Lens' SmallHouse Int
lExaggeratedOne = lFirstFloor . lLeftFlat . lFlatOwner . lPersBirthDate . lYear

lExaggeratedTwo :: Lens' SmallHouse (Int, Int)
lExaggeratedTwo = lGroundFloor . lRightFlat . lFlatSize

lExaggeratedThree :: Lens' SmallHouse String
lExaggeratedThree = lFirstFloor . lRightFlat . lFlatOwner . lLastName
