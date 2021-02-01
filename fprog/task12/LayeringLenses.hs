import Lens

--
-- Layering example
--
data F = F Char [Char] deriving Show
data D = D String Int F deriving Show

lfc :: Lens' F [Char]
lfc fn (F c st) = fmap (\st' -> F c st') (fn st)

lfd :: Lens' D F
lfd fn (D s n f) = fmap (\f' -> D s n f') (fn f)

ldc :: Lens' D [Char]
ldc = lfd . lfc
