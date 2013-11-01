module Pone.PFunctor where

import Pone.Option

class PFunctor f where
    fmap :: (a -> b) -> f a -> f b
    
instance PFunctor Option where
    fmap f o = case o of 
                   Some o -> Some $ f o
                   None -> None
                   
instance PFunctor [] where
    fmap = map