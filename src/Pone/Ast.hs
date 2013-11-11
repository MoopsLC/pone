module Pone.Ast where

import Data.Map as Map

--distinguish between type and *name* of a type
type TypeCtorName = String
type TypeVariable = String
type TypeName = String -- TypeCtorName | TypeVariable
type IdentifierName = String
data PoneProgram t = Program [GlobalDef t] (Expr t)

instance Show (PoneProgram t) where
  show a = "test"

data Location = Location Int    --line number
                         Int    --column
                         String --line

data TypeCtor = TypeCtor TypeCtorName --name
                         [TypeName]   --type parameters

data GlobalDef t = TypeDef TypeCtorName
                           [TypeVariable]
                           [TypeCtor] --constructors
                 | InterfaceDef TypeCtor       --ctor
                                [t]            --inherited types
                                [Definition t] --members
                 | GlobalFunction (Definition t)
                 | ImplementationDef TypeCtorName --name
                                     TypeCtorName --interface
                                     [Definition t]
                 | DefSource Location (GlobalDef t)

data Definition t = Definition IdentifierName   --name
                               [IdentifierName] --formal parameters
                               t                --return type of the function
                               [Constraint t]   --constraints on type
                               (Maybe (Expr t)) --function body, if not abstract

data Constraint t = Constraint TypeVariable t

data Value = PoneInteger Integer
           | PoneFloat Double
           | PoneString String
    deriving (Show, Eq)

data Expr t = Identifier IdentifierName t
            | TypeIdentifier TypeCtor t --isnt this just a lambda?
            | Literal Value t
            | Lambda IdentifierName (Expr t)
            | Apply (Expr t) (Expr t)
            | Source Location (Expr t)
            | Unknown

data Type = Bottom
