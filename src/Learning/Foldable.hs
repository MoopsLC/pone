module Learning.Foldable
( Foldable
, foldMap
) where 

import Learning.Semigroup
import Learning.Monoid
import Learning.Option
import Learning.PFunctor

class Foldable t where
    foldMap :: Monoid m => (a -> m) -> t a -> m
    
    fold :: Monoid m => t m -> m
    fold xs = foldMap id xs
    
instance Foldable [] where
    foldMap f xs = foldl (<+>) zero $ map f xs

{- todo
instance Foldable Option where
-}

    

