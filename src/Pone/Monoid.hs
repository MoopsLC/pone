module Pone.Monoid 
( Monoid
, mappend
, mzero
) where 


{- laws
  forall a. mappend mzero a === mappend a mzero
  forall a b c , mappend a (mappend b c) = mappend (mappend a b) c
-}
class Monoid a where
    mappend :: a -> a -> a
    mzero :: a
    
instance Monoid Int where
    mappend x y = x + y
    mzero = 0
    