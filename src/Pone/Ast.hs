module Pone.Ast where

import Data.Map as Map

import Pone.Environment
import Pone.Writer

data TypeDef = TypeDef String [String] deriving Show
data Environment = Environment { _names :: Map.Map String Expr
                               , _types :: Map.Map String TypeDef
                               } deriving Show

{-

  Grammar:

    <program> ::= (<global-def> *) <expr>

    <global-def> :: = (<type-bind> | <procedure-bind> | <const-bind>) ";"

    <expr> ::= <local-const-bind>
             | <local-procedure-bind>
             | "(" <expr> ")"
             | <procedure-eval>
             | <value-ident>
             | <integer>
             | <float>
             | <lambda>
             | <pattern-matching>

    <const-bind> ::= "define" <value-ident> "as" <expr> "in" <expr>

    <local-const-bind> ::= <const-bind> "in" <expr>

    <procedure-bind> ::= "define" <value-ident> <arg> (<arg> *) "as" <expr>

    <local-procedure-bind> ::= <procedure-bind> "in" <expr>

    <type-bind> ::= "type" <type-ident> "is" <type-def>

    <type-def> ::=  <type-ident> ( "|" <type-def> *)

    <value-ident> ::= [a-z][\w]*

    <type-ident> ::= [A-Z][\w]*

    <procedure-eval> ::= <itent> (<args> +)

    <integer> ::= [0-9]+

    <float> ::= [0-9]+ "." [0-9]*

    <lambda> ::= "(" <ident> "->" <expr> ")"

    <pattern-matching> ::= "match" <expr> "with" "(" (<pattern-bind>)+ ")"

    <pattern-bind> ::= "|" <type-ident> "->" <expr>

-}


type TypeIdent = String


data PoneProgram = Program [GlobalDef] Expr
    deriving (Show, Eq)

data GlobalDef = GlobalIdentifierBind IdentifierBind
               | GlobalTypeBind TypeBind

    deriving (Eq)

instance Show GlobalDef where
    show (GlobalIdentifierBind id) = "(Global " ++ (show id) ++ ")"
    show (GlobalTypeBind t) = "(Global " ++ (show t) ++ ")"

data TypeBind = TypeBind String [TypeIdent]
    deriving (Show, Eq)



data Lambda = Lambda String Expr
    deriving (Eq)

instance Show Lambda where
    show (Lambda name expr) = "(\\" ++ name ++ " -> " ++ (show expr) ++ ")"

data IdentifierBind = IdentifierBind String Expr
    deriving (Eq)

instance Show IdentifierBind where
    show (IdentifierBind name value) = "(" ++ name ++ " = " ++ (show value) ++ ")"

data Pattern = Pattern Var Expr
    deriving (Show, Eq)


data Var = PoneInteger Integer
         | PoneFloat Double
         | UserType String
         | Lam Lambda
         | Identifier String
    deriving (Eq)

instance Show Var where
    show (PoneInteger i) = show i
    show (PoneFloat f) = show f
    show (UserType t) = t
    show (Lam lambda) = show lambda
    show (Identifier id) = id

--an expression to be evaluated
data Expr = LocalIdentifierBind IdentifierBind Expr
          | Apply Expr Expr
          | PatternMatch Expr [Pattern]
          | Value Var
    deriving (Eq)

instance Eq (Expr -> Expr) where
    (==) a b = True

instance Show Expr where
    show (LocalIdentifierBind (IdentifierBind name value) expr) = "let " ++ name ++ " = " ++ (show value) ++ " in " ++ "(" ++ (show expr) ++ ")"
    show (Value var) = show var
    show (Apply l r) = "(Apply " ++ (show l) ++ " " ++ (show r) ++ ")"
    show (PatternMatch expr xs) = "todo"

    --