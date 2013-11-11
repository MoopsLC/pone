module Pone.Ast where

import Data.Map as Map

type TypeVariable = String

data PoneProgram t = Program [GlobalDef t] (Expr t)

data Location = Location Int    --line number
                         Int    --column
                         String --line

data TypeCtor = TypeCtor String   --name
                         [String] --type parameters

data GlobalDef t = TypeDef String     --name
                           [t]        --type parameters
                           [TypeCtor] --constructors
                 | InterfaceDef String         --name
                                [t]            --type parameters
                                [t]            --inherited types
                                [Definition t] --members
                 | GlobalDef (Definition t)

data Definition t = Definition String           --name
                               [String]         --formal parameters
                               t                --return type of the function
                               [Constraint t]   --constraints on type
                               (Maybe (Expr t)) --function body, if not abstract

data Constraint t = Constraint TypeVariable t

data Value = PoneInteger Int
           | PoneFloat Double
           | PoneString String


data Expr t = Identifier String t
            | TypeIdentifier String t
            | Literal Value t
            | Lambda String (Expr t)
            | Apply (Expr t) (Expr t)
            | Source Location (Expr t)
            | Unknown

data Type = Bottom
