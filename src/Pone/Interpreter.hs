{-# LANGUAGE TemplateHaskell #-}
module Pone.Interpreter (poneEval) where
import qualified Data.Map as Map

import Debug.Trace
import Data.Monoid
import Control.Lens
import Control.Exception
import Control.Monad

--import System.IO.Unsafe

import Pone.Ast
import Pone.Parser    
      
type RuntimeError = String
      
data ProcedureDef = ProcedureDef String [String] Expr deriving Show
data TypeDef = TypeDef String [String] deriving Show
data Environment = Environment { _names :: Map.Map String Typed
                               , _procs :: Map.Map String ProcedureDef
                               , _types :: Map.Map String TypeDef
                               } deriving Show

makeEnv = Environment Map.empty Map.empty Map.empty
            
makeLenses ''Environment 

pushProc :: Environment -> String -> ProcedureDef -> Environment
pushProc env name def = procs %~ Map.insert name def $ env

lookupProc :: Environment -> String -> Maybe ProcedureDef
lookupProc env name = Map.lookup name (env ^. procs)

pushName :: Environment -> String -> Typed -> Environment
pushName env name value = names %~ Map.insert name value $ env

lookupName :: Environment -> String -> Maybe Typed
lookupName env name = Map.lookup name (env ^. names)

pushType :: Environment -> String -> TypeDef -> Environment
pushType env name type' = types %~ Map.insert name type' $ env

lookupType :: Environment -> String -> Maybe TypeDef
lookupType env name = Map.lookup name (env ^. types)
    
bind :: Environment -> GlobalDef -> Either RuntimeError Environment
bind env def = case def of
    GlobalProcedureBind (ProcedureBind name parameters body) ->
        return $ pushProc env name $ ProcedureDef name parameters body
    GlobalIdentifierBind (IdentifierBind name value) -> do
        evaluated <- eval env value
        return $ pushName env name evaluated
    GlobalTypeBind (TypeBind name ctors) -> return env

envMultiBind :: Environment -> [(String, Typed)] -> Environment
envMultiBind env [] = env
envMultiBind env ((param, value):xs) = envMultiBind (pushName env param value) xs

eval :: Environment -> Expr -> Either RuntimeError Typed
eval env expr = case expr of
    Value literal -> return literal               
    LocalIdentifierBind (IdentifierBind name v) e -> do
        value <- eval env v
        let newEnv = (pushName env name value) in eval newEnv e
        
        
    LocalProcedureBind (ProcedureBind name args value) expr ->  --make sure no repeat args
        let newEnv = pushProc env name (ProcedureDef name args value) in 
        eval newEnv expr
        
    IdentifierEval s -> case lookupName env s of
        Just value -> return value
        Nothing -> Left ("Unbound name: " ++ (show s))
    
    ProcedureEval name args -> case name of
        "add" -> do v1 <- eval env (args !! 0) --fixme, 
                    v2 <- eval env (args !! 1)
                    case (v1, v2) of 
                        (PoneInteger i, PoneInteger j) -> return $ PoneInteger (i + j)
                        _ -> Left ("type error")
        "toInt" -> do float <- eval env (args !! 0)
                      case float of 
                          PoneInteger i -> Left ("type error")
                          PoneFloat f -> return $ PoneInteger $ truncate f 
        other -> do
            -- [Either String Typed] -> Either String [Typed]
            evaluated :: [Typed] <- sequence $ map (eval env) args-- :: [Either String Typed]
            case lookupProc env other of 
                Just (ProcedureDef other params expr) ->
                    let zipped :: [(String, Typed)] = zip params evaluated -- make sure same length
                        newEnv :: Environment = envMultiBind env zipped 
                    in eval newEnv expr
                Nothing -> Left $ "Unbound procedure: " ++ (show other) ++ (show expr)



tryAny :: IO a -> IO (Either SomeException a)
tryAny = Control.Exception.try

showException :: Either SomeException a -> Either String a
showException (Left exc) = Left $ ("Interpreter: " ++) $ show exc
showException (Right x) = Right x

poneEval :: PoneProgram -> IO (Either RuntimeError Typed)
poneEval (Program globals expr) = 
    let env :: Either RuntimeError Environment = foldM bind makeEnv globals
    in case env of 
        Left err -> return $ Left err
        Right e -> do
            r <- fmap showException $ tryAny $ evaluate $ eval e expr      
            evaluate $ join $ r
    
printInline :: Show a => a -> b -> b
printInline a b = (trace (show a)) b
