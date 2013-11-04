module Pone.Interpreter (poneEval) where
import qualified Data.Map as Map

import Debug.Trace

import Pone.Ast
import Pone.Parser
          
data ProcedureDef = ProcedureDef String [String] Expr
type ProcedureMap = Map.Map String ProcedureDef
type Environment = Map.Map String Integer

pushDef :: ProcedureMap -> String -> ProcedureDef -> ProcedureMap
pushDef map name def = Map.insert name def map

lookupDef :: ProcedureMap -> String -> ProcedureDef
lookupDef map name = map Map.! name --fixme, handle unbound names

pushName :: Environment -> String -> Integer -> Environment
pushName env name value = Map.insert name value env

lookupName :: Environment -> String -> Integer
lookupName env name = env Map.! name --fixme, handle unbound names

poneEval :: PoneProgram -> Integer
poneEval (Program globals expr) = 
    let (env, procs) = foldl (\acc def -> bind acc def) (Map.empty, Map.empty) globals
    in eval env procs expr
    



bind :: (Environment,ProcedureMap) -> GlobalDef -> (Environment, ProcedureMap)
bind (env, procs) def = case def of
    GlobalProcedureBind (ProcedureBind name parameters body) ->
        (env, pushDef procs name (ProcedureDef name parameters body))
    GlobalIdentifierBind (IdentifierBind name value) -> 
        let evaluated = eval env procs value in
        ((pushName env name evaluated), procs)
       


mapFold :: Ord k => Map.Map k v -> [(k, v)] -> Map.Map k v
mapFold map [] = map
mapFold map ((param, value):xs) = mapFold (Map.insert param value map) xs


    
eval :: Environment -> ProcedureMap -> Expr -> Integer
eval env procs expr = case expr of
    Value (PoneInteger i) -> i 
    Binop op e1 e2 -> 
        let v1 = eval env procs e1
            v2 = eval env procs e2
        in case op of 
            Plus -> v1 + v2
            Times -> v1 * v2
                                 
    LocalIdentifierBind (IdentifierBind name v) e -> 
        let value = eval env procs v
            newState = pushName env name value
        in eval newState procs e
    LocalProcedureBind (ProcedureBind name args value) expr ->  --make sure no repeat args
        let newProcs = pushDef procs name (ProcedureDef name args value) in 
        eval env newProcs expr
    IdentifierEval s -> lookupName env s
    ProcedureEval name args -> 
        let evaluated :: [Integer] = map (eval env procs) args
            procc = lookupDef procs name
        in case procc of 
            ProcedureDef name params expr ->
                let zipped :: [(String, Integer)] = zip params evaluated -- make sure same length
                    newEnv :: Environment = mapFold env zipped 
                in eval newEnv procs expr
                
