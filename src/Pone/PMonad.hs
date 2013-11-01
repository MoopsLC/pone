module Pone.PMonad where

import Pone.Option
import Pone.Applicative 

class Applicative m => PMonad m where
    (>>=) :: m a -> (a -> m b) -> m b  
    
instance PMonad Option where
    None >>= _  = None
    (Some x) >>= f = f x
    