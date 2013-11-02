module Pone.Semigroup
( Semigroup
, (<+>)
) where 

import Pone.Option

{-
    requires ∀x,y,z ∈ a,  (x <+> y) <+> z === x <+> (y <+> z)
-}
class Semigroup a where
    (<+>) :: a -> a -> a
    
instance Semigroup Int where
    (<+>) = (+)
    
instance Semigroup [a] where 
    (<+>) = (++)
    
instance Semigroup (a -> a) where 
    (<+>) = (.)
    
instance Semigroup a => Semigroup (Option a) where
    None <+> Some x = Some x
    Some x <+> None = Some x
    Some x <+> Some y = Some (x <+> y)
    