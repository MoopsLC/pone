module Pone.Ast where

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
               
    deriving (Show, Eq)

data TypeBind = TypeBind String [TypeIdent]
    deriving (Show, Eq)
    
data Lambda = Lambda String Expr
    deriving (Show, Eq)

data IdentifierBind = IdentifierBind String Expr
    deriving (Show, Eq)
   
data Pattern = Pattern Var Expr
    deriving (Show, Eq)


data Var = PoneInteger Integer
         | PoneFloat Double
         | UserType String
         | Lam Lambda
         | Identifier String
    deriving (Show, Eq)

--an expression to be evaluated
data Expr = LocalIdentifierBind IdentifierBind Expr 
          | Apply !Expr !Expr --bind first Expr to the name String in Expr
          | PatternMatch Expr [Pattern]
          | Value Var
    deriving (Show, Eq)
    
    