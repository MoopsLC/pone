module Learning.Applicative where
import Learning.PFunctor
import Learning.List 
import Learning.Option
import System.IO

{-
    laws: pure f <*> x = pfmap f x
          pure id <*> v = v
          pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
          u <*> pure y = pure ($ y) <*> u
-}

class PFunctor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b 
    
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [ f x | f <- fs, x <- xs ]
    
instance Applicative Option where
    pure = Some
    None <*> _ = None
    Some f <*> None = None
    Some f <*> opt = f <$$> opt 
    
instance Applicative IO where 
    pure = return
    fs <*> xs = do f <- fs
                   x <- xs
                   return $ f x
                
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = f <$$> a

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$$> a <*> b


{- i don't understand this
instance Applicative ((->) r) where
    pure x = 
    f <*> g = 
-}