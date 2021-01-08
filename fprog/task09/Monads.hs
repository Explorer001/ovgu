import Control.Monad
--
-- 9.1.1 ID
--
data Identity a = Identity a deriving (Show)

instance Monad Identity where
    (Identity x) >>= f = f x
    return             = Identity

instance Applicative Identity where
    pure  = return
    (<*>) = ap

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

--
-- 9.1.2 Monad counting binds
--
data CountBinds a = CountBinds (Integer, a)

binds :: CountBinds a -> Integer
binds (CountBinds x) = fst(x)

instance Monad CountBinds where
    return x = CountBinds (0, x)
    (CountBinds (x,y)) >>= f = let CountBinds (a, b) = f y in CountBinds (x+a + 1, b)

instance Applicative CountBinds where
    pure = return
    CountBinds (x1, y1) <*> CountBinds (x2, y2) = CountBinds (x1 + x2 + 1, y1 y2)

instance Functor CountBinds where
    fmap f (CountBinds (x,y)) = CountBinds (x, f y)

two :: CountBinds Int
two = do
    x <- return 1
    y <- return 2
    return (x+y)
