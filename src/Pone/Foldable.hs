module Pone.Foldable
( Foldable
, foldMap
) where 

import Pone.Semigroup
import Pone.Monoid

class Foldable t where
    foldMap :: Monoid m => (a -> m) -> t a -> m
    fold :: Monoid m => t m -> m
    fold xs = foldMap id xs
    
instance Foldable [] where
    foldMap f xs = foldl (<+>) zero $ map f xs
    

