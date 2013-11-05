{-# LANGUAGE NoMonomorphismRestriction #-}
import System.IO
import Data.Monoid

import qualified Data.Map as Map

import Pone.Parser
import Pone.Interpreter

import Debug.Trace

main = checkSource "C:/Users/M/Desktop/pone/pone_src/test2.pone" 8
       
testFile :: String -> IO (Maybe Integer)
testFile filename = do
    source <- readFile filename
    return $ case parsePone source of 
       Left err -> trace (show err) Nothing
       Right ast -> Just $ poneEval ast
       
      
checkSource :: String -> Integer -> IO (Maybe Bool)
checkSource filename result = do
    evaluated <- testFile filename
    return $ do
        value <- evaluated
        return $ value == result


    
    
