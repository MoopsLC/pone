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
import qualified Data.Map as Map

import Pone.Ast
import Pone.Parser
          
type Environment = Map.Map String Integer

pushName :: Environment -> Integer -> String -> Environment
pushName env value name = Map.insert name value env

lookupName :: Environment -> String -> Integer
lookupName env name = env Map.! name --fixme, handle unbound names

eval :: Environment -> Expr -> Integer
eval env (Assign name v e) = let value = eval env v
                                 newState = pushName env value name 
                              in eval newState e
eval env (Value i) = i
eval env (Binop op e1 e2) = let v1 = eval env e1
                                v2 = eval env e2
                             in case op of 
                                 Plus -> v1 + v2
                                 Times -> v1 * v2
eval env (Paren e) = eval env e
eval env (Identifier s) = lookupName env s
eval env (Procedure name args value expr) = eval env expr
   

main = do
    source <- readFile "C:/Users/M/Desktop/pone/pone_src/test.pone"
    case parsePone source of 
       Left err -> print err
       Right ans -> print $ eval (Map.empty) ans
    



       