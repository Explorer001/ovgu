{-# LANGUAGE RankNTypes #-}

module Lens
( Lens'
, view
, set
) where

--
-- Simplified Lens type
--
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

newtype Const v a = Const { getConst :: v }
instance Functor (Const v) where
    fmap f (Const x) = Const x

view :: Lens' s a -> s -> a
view ln = getConst . ln Const

newtype Identity a = Identity { runIdentity :: a }
instance Functor Identity where
    fmap f (Identity s) = Identity (f s)

set :: Lens' s a -> a -> s -> s
set ln x = runIdentity . ln (Identity . const x)
