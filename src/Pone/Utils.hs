module Pone.Utils where

import Debug.Trace

printInline :: Show a => a -> b -> b
printInline x y = (trace (show x)) y

printInlineStr :: String -> b -> b
printInlineStr x y = (trace x) y

(<::>) :: a -> a -> [a] -> [a]
(<::>) x y xs = x:y:xs

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = ((.) . (.))

-- | ignore dot files
isFile :: String -> Bool
isFile [] = False
isFile ('.':[]) = False
isFile ('.':'.':xs) = False
isFile _ = True

maybeToEither :: Maybe a -> b -> Either b a
maybeToEither Nothing y = Left y
maybeToEither (Just x) y = Right x
