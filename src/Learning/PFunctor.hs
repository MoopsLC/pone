module Learning.PFunctor where

import Learning.Option

{-
    requires (<$$>) id === id
             (<$$>) (f . g) == (<$$>) f . (<$$>) g
-}
class PFunctor f where
    (<$$>) :: (a -> b) -> f a -> f b
    
instance PFunctor Option where
    (<$$>) f o = case o of 
                 Some o -> Some $ f o
                 None -> None
                   
instance PFunctor [] where
    (<$$>) = map
    
instance PFunctor ((->) r) where
    (<$$>) = (.)
    
instance PFunctor IO where 
    (<$$>) f o = do value <- o
                    return $ f value
    