import Pone.Tree
import Pone.Semigroup
import Pone.Monoid
import Pone.Foldable
import Pone.List
import Pone.Applicative
import Pone.Option
import Pone.PMonad
import System.IO


print' :: (Show a) => a -> IO ()
print' a = putStrLn $ show a


{- monad promised land -}
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
    
main = do  
    print $ liftA2 (:) (Some 3) (Some [4]) 
    
