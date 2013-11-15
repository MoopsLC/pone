module Pone.Ast where

import qualified Data.Map as Map

import Pone.Pretty

type TypeCtorName = String
type TypeVariable = String
type TypeName = String
type IdentifierName = String

data PoneProgram t = Program [GlobalDef t] (Expr t)
    deriving (Show)

data Location = Location Int    --line number
                         Int    --column
                         String --line
    deriving (Show)

data CompoundType = CompoundType TypeCtorName --name
                                 [TypeName]   --type parameters
    deriving (Show)

data GlobalDef t = TypeDef TypeCtorName
                           [TypeVariable]
                           [CompoundType] --constructors
                 | InterfaceDef CompoundType       --ctor
                                [CompoundType]     --inherited types
                                [Definition t] --members
                 | GlobalFunction (Definition t)
                 | ImplementationDef CompoundType --name
                                     CompoundType --interface
                                     [Constraint t]
                                     [Definition t]
                 | DefSource Location (GlobalDef t)
    deriving (Show)

data Definition t = Definition IdentifierName   --name
                               [IdentifierName] --formal parameters
                               t                --return type of the function
                               [Constraint t]   --constraints on type
                               (Maybe (Expr t)) --function body, if not abstract
    deriving (Show)

data Constraint t = Constraint TypeVariable t
    deriving (Show)

data Value = PoneInteger Integer
           | PoneFloat Double
           | PoneString String
    deriving (Eq, Show)

data PatternBranch t = Branch Pattern (Expr t)
  deriving (Show)

data Pattern = TypePattern CompoundType
             | LiteralPattern Value
             | IdentifierPattern String
    deriving (Show)

data Expr t = Identifier IdentifierName t
            | Literal Value t
            | PatternMatch (Expr t) [PatternBranch t]
            | Lambda IdentifierName (Expr t)
            | Apply (Expr t) (Expr t)
            | LocalDefine (Definition t) (Expr t)
            | Source Location (Expr t)
    deriving (Show)

data Type k = ProdT (Type k) (Type k)
            | TypeValue TypeName k
            | Arrow
            | UnknownT
    deriving (Show)

data Kind = ArrowK Kind Kind
          | Star
          | UnknownK
    deriving (Show)

getExpr :: PoneProgram t -> Expr t
getExpr (Program _ e) = e

stripSource :: PoneProgram t -> PoneProgram t
stripSource (Program defs expr) = Program defs $ stripExpr expr
-- | remove source information
stripExpr :: Expr t -> Expr t
stripExpr expr = case expr of
    Source loc e -> stripExpr e
    e -> e

data ApplyList t = ApplyList [t]

listToApply :: (t -> t -> t)->  ApplyList t -> t
listToApply f (ApplyList xs) = foldl1 f xs
--a b c d === (Apply (Apply (Apply a b) c) d)

instance Pretty t => Pretty (PoneProgram t) where
    pretty (Program defs expr) = (pretty defs) ++ "\n" ++ (pretty expr)

instance Pretty CompoundType where
    pretty (CompoundType ctor names) = ctor ++ " " ++ (join " " names)

printConstraints :: [Constraint t] -> String
printConstraints [] = ""
printConstraints xs = joinList [" where (", (joinPretty ", " xs), ")"]

instance Pretty t => Pretty (GlobalDef t) where
    pretty (TypeDef ctor names ts) =
        joinList [ "type "
                 , ctor
                 , " "
                 , join " " names
                 , " is "
                 , joinPretty "| " ts
                 ]
    pretty (InterfaceDef name inhs defs) =
        joinList [ "interface "
                 , pretty name
                 , pinhs
                 , "is \n"
                 , pretty defs
                 , " end\n"
                 ]
        where pinhs = case inhs of
                          [] -> ""
                          xs -> joinList [ " extends ", (joinPretty ", " xs)]
    pretty (GlobalFunction def) = pretty def
    pretty (ImplementationDef name inh constrs defs) =
        joinList [ "implement "
                 , pretty name
                 , " for "
                 , pretty inh
                 , printConstraints constrs
                 , " as "
                 , pretty defs
                 , " end\n"
                 ]
    pretty (DefSource loc def) = pretty def

instance Pretty t => Pretty (Definition t) where
    pretty (Definition name params t constrs expr) =
        joinList [ "define "
                 , name
                 , " "
                 , join " " params
                 , " : "
                 , pretty t
                 , " as "
                 , printConstraints constrs
                 , maybe "" pretty expr
                 , maybe "abstract\n" (\x -> "end\n") expr
                 ]

instance Pretty (Constraint t) where
  pretty (Constraint var t) = var ++ " < fixme"

instance Pretty Value where
    pretty (PoneInteger i) = show i
    pretty (PoneFloat f) = show f
    pretty (PoneString s) = show s

instance Pretty t => Pretty (PatternBranch t) where
    pretty (Branch pat expr) = "| " ++ (pretty pat) ++ " -> " ++ (pretty expr) ++ "\n"


instance Pretty Pattern where
    pretty (TypePattern t) = pretty t
    pretty (LiteralPattern v) = pretty v
    pretty (IdentifierPattern i) = i

instance Pretty t => Pretty (Expr t) where
    pretty (Lambda name expr) = "[λ " ++ name ++ " . " ++ (pretty expr) ++ "]"
    pretty (Apply e0 e1) = (pretty e0) ++ " " ++ (pretty e1)
    pretty (Source loc expr) = pretty expr
    pretty (Literal v t') = pretty v
    pretty (LocalDefine def expr) = pretty def ++ " in " ++ (pretty expr) ++ "\n"
    pretty (Identifier id' t') = id'
    pretty (PatternMatch expr pats) =
        "match " ++ (pretty expr) ++ " with " ++ (pretty pats) ++ " end\n"

instance Pretty (Type k) where
    pretty (ProdT (ProdT Arrow t0) t1) = "(" ++ (pretty t0) ++ " -> " ++ (pretty t1) ++ ")"
    pretty (ProdT t0 t1) = "(" ++ (pretty t0) ++ " " ++ (pretty t1) ++ ")"
    pretty Arrow = "(->)"
    pretty UnknownT = "⊥"
    pretty (TypeValue string k) = string

instance Pretty Kind where
    pretty (ArrowK k0 k1) = (pretty k0) ++ " => " ++ (pretty k0)
    pretty Star = "•"
    pretty UnknownK = "¿"
