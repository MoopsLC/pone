module Pone.Ast where

import qualified Data.Map as Map

--distinguish between type and *name* of a type
type TypeCtorName = String
type TypeVariable = String
type TypeName = String -- TypeCtorName | TypeVariable
type IdentifierName = String

data PoneProgram t = Program [GlobalDef t] (Expr t)

instance Show (PoneProgram t) where
  show (Program defs expr) = "Program: "

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
    deriving (Show, Eq)

data Expr t = Identifirer IdentifierName t
            | TypeIdentifier TypeCtor t --isnt this just a lambda?
            | Literal Value t
            | Lambda IdentifierName (Expr t)
            | Apply (Expr t) (Expr t)
            | Source Location (Expr t)
            | Unknown
    deriving (Show)

data Type k = Prod (Type k){-must be TypeValue, refactor-} (Type k)
            | Arrow (Type k) (Type k)
            | TypeValue TypeName k
            | UnknownT
    deriving (Show)

data Kind = UnknownK
    deriving (Show)

bottom :: Type k
bottom = UnknownT

data ArrowList k = ArrowList (Type k) [Type k]
listToArrow :: ArrowList k -> Type k
listToArrow (ArrowList t0 ts) = foldl Arrow t0 ts
-- a -> b -> c -> d === a -> (b -> (c -> d)))