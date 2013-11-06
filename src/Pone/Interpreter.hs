{-# LANGUAGE TemplateHaskell #-}
module Pone.Interpreter (poneEval) where
import qualified Data.Map as Map

import Debug.Trace
import Control.Lens
import Control.Exception
import Control.Monad

import System.IO.Unsafe

import Pone.Ast
import Pone.Parser    
      
type RuntimeError = String
      
data ProcedureDef = ProcedureDef String [String] Expr deriving Show

data Environment = Environment { _names :: Map.Map String Integer
                               , _procs :: Map.Map String ProcedureDef
                               } deriving Show

makeEnv = Environment Map.empty Map.empty
            
makeLenses ''Environment 

pushProc :: Environment -> String -> ProcedureDef -> Environment
pushProc env name def = procs %~ Map.insert name def $ env

lookupProc :: Environment -> String -> Maybe ProcedureDef
lookupProc env name = Map.lookup name (env ^. procs)

pushName :: Environment -> String -> Integer -> Environment
pushName env name value = names %~ Map.insert name value $ env

lookupName :: Environment -> String -> Maybe Integer
lookupName env name = Map.lookup name (env ^. names)

tryAny :: IO a -> IO (Either SomeException a)
tryAny = Control.Exception.try

showException :: Either SomeException a -> Either String a
showException (Left exc) = Left $ ("Interpreter: " ++) $ show exc
showException (Right x) = Right x

poneEval :: PoneProgram -> IO (Either RuntimeError Integer)
poneEval (Program globals expr) = 
    let env :: Either RuntimeError Environment = foldM bind makeEnv globals
    in case env of 
        Left err -> return $ Left err
        Right e -> do
            r <- fmap showException $ tryAny $ evaluate $ eval e expr      
            evaluate $ join $ r
    
printInline :: Show a => a -> b -> b
printInline a b = (trace (show a)) b
    
bind :: Environment -> GlobalDef -> Either RuntimeError Environment
bind env def = case def of
    GlobalProcedureBind (ProcedureBind name parameters body) ->
        return $ pushProc env name $ ProcedureDef name parameters body
    GlobalIdentifierBind (IdentifierBind name value) -> do
        evaluated <- eval env value
        return $ pushName env name evaluated


envMultiBind :: Environment -> [(String, Integer)] -> Environment
envMultiBind env [] = env
envMultiBind env ((param, value):xs) = envMultiBind (pushName env param value) xs

eval :: Environment -> Expr -> Either RuntimeError Integer
eval env expr = case expr of
    Value (PoneInteger i) -> Right i 
    Binop op e1 e2 -> do
        v1 <- eval env e1
        v2 <- eval env e2
        return $ case op of 
            Plus -> v1 + v2
            Times -> v1 * v2
                                 
    LocalIdentifierBind (IdentifierBind name v) e -> do
        value <- eval env v
        let newEnv = (pushName env name value) in eval newEnv e
        
        
    LocalProcedureBind (ProcedureBind name args value) expr ->  --make sure no repeat args
        let newEnv = pushProc env name (ProcedureDef name args value) in 
        eval newEnv expr
        
    IdentifierEval s -> case lookupName env s of
        Just value -> return value
        Nothing -> Left ("Unbound name: " ++ (show s))
    
    ProcedureEval name args -> do
        -- [Either String Integer] -> Either String [Integer]
        evaluated :: [Integer] <- sequence $ map (eval env) args-- :: [Either String Integer]
        case lookupProc env name of 
            Just (ProcedureDef name params expr) ->
                let zipped :: [(String, Integer)] = zip params evaluated -- make sure same length
                    newEnv :: Environment = envMultiBind env zipped 
                in eval newEnv expr
            Nothing -> Left $ "Unbound procedure: " ++ (show name) ++ (show expr)

--failFirst :: [Either a b] -> Either a [b]  
