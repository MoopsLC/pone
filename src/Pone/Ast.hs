module Pone.Ast where

import qualified Data.Map as Map

--distinguish between type and *name* of a type
type TypeCtorName = String
type TypeVariable = String
type TypeName = String -- TypeCtorName | TypeVariable
type IdentifierName = String

data PoneProgram t = Program [GlobalDef t] (Expr t)

getExpr :: PoneProgram t -> Expr t
getExpr (Program _ e) = e

class Pretty a where
    pretty :: a -> String

instance Show (PoneProgram (Type Kind)) where
    show (Program defs expr) = "Program: " ++ (show defs) ++ (show expr)

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
    deriving (Eq)

instance Show Value where
    show (PoneInteger i) = show i
    show (PoneFloat f) = show f
    show (PoneString s) = show s

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

instance Show (Expr t) where
    show (Lambda name expr) = "[λ " ++ name ++ " . " ++ (show expr) ++ "]"
    show (Apply e0 e1) = (show e0) ++ " " ++ (show e1)
    show (Source loc expr) = show expr
    show (Literal v t') = show v
    show (LocalDefine def expr) = "todo"
    show (Identifier id' t') = id'
    show (PatternMatch expr pats) =
        "match " ++ (show expr) ++ " with " ++ (show pats) ++ " end"

data ApplyList t = ApplyList [t]

listToApply :: (t -> t -> t)->  ApplyList t -> t
listToApply f (ApplyList xs) = foldl1 f xs
--a b c d === (Apply (Apply (Apply a b) c) d)

data Type k = ProdT (Type k) (Type k)
            | TypeValue TypeName k
            | Arrow
            | UnknownT

instance Show (Type k) where
  show (ProdT t0 t1) = "(" ++ (show t0) ++ " " ++ (show t1) ++ ")"
  show Arrow = "(->)"
  show UnknownT = "⊥"
  show (TypeValue string k) = string

data Kind = ProdK Kind Kind
          | Star
          | UnknownK
    deriving (Show)
