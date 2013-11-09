module Pone.Utils where

import Debug.Trace

printInline :: Show a => a -> b -> b
printInline a b = (trace (show a)) b
