module Learning.PMonad where

import Learning.Option
import Learning.Applicative 
import Learning.List

{-
    laws: (m >>= f) >>= g === m >>= (\x -> (f x >>= g))
          (return x) >>= f === f x
          
-}

class Applicative m => PMonad m where
    (>>=) :: m a -> (a -> m b) -> m b  
    
    mreturn :: a -> m a
    mreturn = pure
    
    --(>>) :: m a -> m b -> m b
    --(>>)
    
    
instance PMonad Option where
    None >>= _  = None
    (Some x) >>= f = f x
    
instance PMonad [] where
    [] >>= _ = []
    xs >>= f = flatten' (map f xs)
    
