module Pone.Ast where

{-

  Grammar:

    <program> ::= (<global-def> *) <expr>

    <global-def> :: = <type-bind> | <procedure-bind> | <const-bind>
    
    <expr>    ::= <local-const-bind>
                | <local-procedure-bind>
                | "(" <expr> ")"
                | <expr> <binop> <expr>
                | <procedure-eval>
                | <pattern-match>
                | <value-ident>
                | <integer>

    <const-bind> ::= "define" <value-ident> "as" <expr> "in" <expr>
    
    <local-const-bind> ::= <const-bind> "in" <expr>
    
    <procedure-bind> ::= "define" <value-ident> <arg> (<arg> *) "as" <expr>
    
    <local-procedure-bind> ::= <procedure-bind> "in" <expr>
           
    <binop> ::= "+" | "*"

    <pattern> ::= ??? "in"
    
    <pattern-match> ::= "see" <expr> "as" <pattern> (<pattern> *)
    
    <type-bind> ::= "type" <type-def>
    
    <type-def> ::=  <type-ident> (<ident> *)
    
    <value-ident> ::= [a-z][\w]*
    
    <type-ident> ::= [A-Z][\w]*
    
    <procedure-eval> ::= <itent> (<args> +)

    <integer> ::= [0-9]+

-}


data PoneProgram = Program {-[GlobalDef]-} Expr

-- data GlobalDef = GlobalProcedureBind ProcedureBind
               -- | 
    -- deriving Show
    
data Op = Plus | Times deriving Show

data Typed = PoneInteger Integer 
           | PoneBoolean Bool 
   deriving Show

data ProcedureBind = ProcedureBind String [String] Expr Expr
   deriving Show
   
data Expr = Value Typed
          | Binop Op Expr Expr
          | IdentifierBind String Expr Expr 
          | IdentifierEval String
          | LocalProcedureBind ProcedureBind
          | ProcedureEval String [Expr]
    deriving Show
    
    