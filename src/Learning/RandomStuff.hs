module Learning.RandomStuff where

import Data.Monoid
import System.IO

print' :: (Show a) => a -> IO ()
print' a = putStrLn $ show a


sequence' :: [IO a] -> IO [a]
sequence' [] = return  []
sequence' (x:xs) = do 
    v <- x
    vs <- sequence xs
    return (v:vs)

mapM' :: (a -> IO b) -> [a] -> IO [b]
mapM' f xs = sequence' $ map f xs

forM' :: [a] -> (a -> IO b) -> IO [b]
forM' xs f = sequence' $ map f xs

mapM_' :: (a -> IO b) -> [a] -> IO ()

mapM_' f xs = do 
    sequence' $ map f xs
    return ()
    
--todo
--forever' :: IO a -> IO ()
--forever' a = do _ <- a ; forever' a ; return ()

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    file <- openFile path mode
    result <- f file
    hClose file
    return result
    
readFile' :: FilePath -> IO String
readFile' path = withFile' path ReadMode hGetContents
    
thing :: Maybe Int
thing = do
    start <- Nothing
    thing2 <- Just (start + 100)
    return start
    

applyLog :: Monoid c => (a, c) -> (a -> (b, c)) -> (b, c)  
applyLog (x1, log1) f = let (x2, log2) = f x1 in (x2, log1 `mappend` log2)


newtype Writer w a = Writer { runWriter :: (a, w) }

 
instance Monoid w => Monad (Writer w) where 
    return x = Writer (x, mempty)
    Writer (x, acc) >>= f =  let Writer (x2, acc2) = f x in Writer (x2, acc `mappend` acc2)

    
    
newtype PState s a = PState { runState :: s -> (a,s) }
instance Monad (PState s) where 
    return x = PState (\s -> (x, s))
    --m a -> (a -> m b) -> m b 
    (PState h) >>= f = PState (\state -> let (aa, newState) = h state 
                                             (PState g) = f aa 
                                         in g newState)
                                         
join' :: (Monad m) => m (m a) -> m a
join' mm = do
    m <- mm
    m
    
    
liftM' :: (Monad m) => (a -> b) -> m a -> m b  
liftM' f xs = do
    x <- xs
    return $ f x
    
liftM2' :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2' f x y = do 
    vx <- x
    vy <- y
    return $ f vx vy
    
ap' :: Monad m => m (a -> b) -> m a -> m b  
ap' fs x = do
    f <- fs
    v <- x
    return $ f v

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' p [] = return []
filterM' p (x:xs) = do  
    test <- p x
    rest <- filterM' p xs
    return $ if test then x : rest else rest
    
  
foldM' :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ x [] = return x
foldM' f x (y:ys) = do 
    x' :: a <- f x y
    foldM' f x' ys