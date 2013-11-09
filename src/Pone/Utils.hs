module Utils where

printInline :: Show a => a -> b -> b
printInline a b = (trace (show a)) b
