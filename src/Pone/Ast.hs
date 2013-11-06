module Pone.Ast where

{-

  Grammar:

    <program> ::= (<global-def> *) <expr>

    <global-def> :: = (<type-bind> | <procedure-bind> | <const-bind>) ";"
    
    <expr>    ::= <local-const-bind>
                | <local-procedure-bind>
                | "(" <expr> ")"
                | <procedure-eval>
                | <value-ident>
                | <integer>

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

-}


data PoneProgram = Program [GlobalDef] Expr
    deriving Show
    
data GlobalDef = GlobalProcedureBind ProcedureBind
               | GlobalIdentifierBind IdentifierBind
    deriving Show

data Typed = PoneInteger Integer
    deriving Show

data ProcedureBind = ProcedureBind String [String] Expr 
    deriving Show
   
data IdentifierBind = IdentifierBind String Expr
    deriving Show
   
data Expr = Value Typed
          | LocalIdentifierBind IdentifierBind Expr 
          | IdentifierEval String
          | LocalProcedureBind ProcedureBind Expr
          | ProcedureEval String [Expr]
    deriving Show
    
    