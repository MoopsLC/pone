module Pone.Interpreter (poneEval) where
import qualified Data.Map as Map

import Debug.Trace

import Pone.Ast
import Pone.Parser
          
type Environment = Map.Map String Integer
data ProcedureDef = ProcedureDef String [String] Expr
type ProcedureMap = Map.Map String ProcedureDef

pushDef :: ProcedureMap -> String -> ProcedureDef -> ProcedureMap
pushDef map name def = Map.insert name def map

lookupDef :: ProcedureMap -> String -> ProcedureDef
lookupDef map name = map Map.! name --fixme, handle unbound names

pushName :: Environment -> String -> Integer -> Environment
pushName env name value = Map.insert name value env

lookupName :: Environment -> String -> Integer
lookupName env name = env Map.! name --fixme, handle unbound names

poneEval :: PoneProgram -> Integer
poneEval (Program expr) = eval Map.empty Map.empty expr

eval :: Environment -> ProcedureMap -> Expr -> Integer
eval env procs expr = case expr of
    Value (PoneInteger i) -> i 
    Binop op e1 e2 -> let v1 = eval env procs e1
                          v2 = eval env procs e2
                      in case op of 
                          Plus -> v1 + v2
                          Times -> v1 * v2
                                 
    IdentifierEval s -> lookupName env s
    IdentifierBind name v e -> let value = eval env procs v
                                   newState = pushName env name value
                               in eval newState procs e
    LocalProcedureBind (ProcedureBind name args value expr) ->  --make sure no repeat args
        let newProcs = pushDef procs name (ProcedureDef name args value) in 
        eval env newProcs expr
    ProcedureEval name args -> 
        let evaluated :: [Integer] = map (eval env procs) args
            procc = lookupDef procs name
        in case procc of 
            ProcedureDef name params expr ->
                let zipped :: [(String, Integer)] = zip params evaluated -- make sure same length
                    newEnv :: Environment = mapFold env zipped 
                in eval newEnv procs expr
                where mapFold :: Environment -> [(String, Integer)] -> Environment -- use Foldable
                      mapFold map [] = map
                      mapFold map ((param, value):xs) = mapFold (Map.insert param value map) xs


