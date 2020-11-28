--
-- 5.1 Comprehensible Streams
--
harmonic :: (Fractional a, Enum a) => [a]
harmonic = map (1/) (tail [0 ..])

palin :: [Int]
palin = filter (\x -> show x == reverse (show x)) [0 ..]

partsum :: Num a => [a] -> [a]
partsum xs = [x | x <- scanl (+) 0 xs]
