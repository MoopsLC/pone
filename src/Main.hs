{-# LANGUAGE NoMonomorphismRestriction #-}

import Learning.Tree
import Learning.Foldable
import Learning.List
import Learning.PFunctor
import Learning.Applicative
import Learning.Option
import Learning.RandomStuff
import System.IO
import Data.Monoid


                      
--Paren should not be its own thing

main = do
    source <- readFile "C:/Users/M/Desktop/pone/pone_src/test.pone"
    case parsePone source of 
       Left err -> print err
       Right ans -> print $ eval (Map.empty) (Map.empty) ans
    



       