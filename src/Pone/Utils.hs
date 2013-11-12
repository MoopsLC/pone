module Pone.Utils where

import Debug.Trace

printInline :: Show a => a -> b -> b
printInline a b = (trace (show a)) b

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = ((.) . (.))

isFile :: String -> Bool
isFile [] = False
isFile ('.':[]) = False
isFile ('.':'.':xs) = False
isFile other = True
