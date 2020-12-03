module MVector
( Vector(..)
, vectorFromList
, skalarMult
, vecAdd
, vecProd
, crossProd
) where

import Data.Maybe

data Vector a = Vector [a] deriving (Show)

vectorFromList :: Num a => [a] -> Vector a
vectorFromList l = Vector l

skalarMult :: Num a => a -> Vector a -> Vector a
skalarMult n (Vector v) = Vector ([x * n | x <- v])

vecAdd :: Num a => Vector a -> Vector a -> Maybe (Vector a)
vecAdd (Vector v1) (Vector v2)
    | null v1 = Nothing
    | null v2 = Nothing
    | length v1 /= length v2 = Nothing
    | otherwise = Just (Vector (zipWith (+) v1 v2))

vecProd :: Num a => Vector a -> Vector a -> Maybe a
vecProd (Vector v1) (Vector v2)
    | null v1 = Nothing
    | null v2 = Nothing
    | length v1 /= length v2 = Nothing
    | otherwise = Just (foldr1 (+) (zipWith (*) v1 v2))

crossProd :: Num a => Vector a -> Vector a -> Maybe (Vector a)
crossProd (Vector v1) (Vector v2)
    | length v1 /= 3 = Nothing
    | length v2 /= 3 = Nothing
    | otherwise = let x1:x2:x3:_ = v1
                      y1:y2:y3:_ = v2 in
                    Just (Vector [x2*y3 - x3*y2, x3*y1 - x1*y3, x1*y2 - x2*y1])
