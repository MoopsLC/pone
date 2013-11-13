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

instance Show (PoneProgram (Type Kind)) where
  show (Program defs expr) = "Program: " ++ (show defs) ++ (show expr)

data Location = Location Int    --line number
                         Int    --column
                         String --line
    deriving (Show)

data TypeCtor = TypeCtor TypeCtorName --name
                         [TypeName]   --type parameters
    deriving (Show)

data GlobalDef t = TypeDef TypeCtorName
                           [TypeVariable]
                           [TypeCtor] --constructors
                 | InterfaceDef TypeCtor       --ctor
                                [TypeCtor]     --inherited types
                                [Definition t] --members
                 | GlobalFunction (Definition t)
                 | ImplementationDef TypeCtorName --name
                                     TypeCtorName --interface
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

data Expr t = Identifier IdentifierName t
            | Literal Value t
            | Lambda IdentifierName (Expr t)
            | Apply (Expr t) (Expr t)
            | Source Location (Expr t)
            | Unknown

instance Show (Expr t) where
  show (Lambda name expr) = "[λ " ++ name ++ " . " ++ (show expr) ++ "]"
  show (Apply e0 e1) = (show e0) ++ " " ++ (show e1)
  show (Source loc expr) = show expr
  show Unknown = undefined
  show (Literal v t') = show v
  show (Identifier id' t') = id'

data ApplyList t = ApplyList t [t]

listToApply :: (t -> t -> t)->  ApplyList t -> t
listToApply f (ApplyList x xs) = foldl f x xs
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

data ArrowList k = ArrowList (Type k) [Type k]
--listToArrow :: ArrowList k -> Type k
--listToArrow (ArrowList t0 ts) = foldl ProdT t0 ts

listToArrow :: ArrowList k -> Type k
listToArrow (ArrowList tt tts) = foldl (\x acc -> ProdT (ProdT Arrow acc) x) tt tts
