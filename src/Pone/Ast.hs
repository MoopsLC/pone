module Pone.Ast where

{-

  Grammar:


    <expr>    ::= <const-bind>
                | <procedure-bind>
                | "(" <expr> ")"
                | <expr> <binop> <expr>
                | <procedure-eval>
                | <ident>
                | <integer>
                
               
    <const-bind> ::= "define" <ident> "as" <expr> "in" <expr>

    <procedure-bind> ::= <ident> <arg> (<arg> *) "is" <expr>
           
    <binop> ::= "+" | "*"

    <ident> ::= [a-zA-Z][\w]*
    
    <procedure-eval> ::= <itent> (<args> +)

    <integer> ::= [0-9]+

-}


data Op = Plus | Times deriving Show
data Expr = IdentifierBind String Expr Expr 
          | Value Integer 
          | Binop Op Expr Expr
          | IdentifierEval String
          | ProcedureBind String [String] Expr Expr
          | ProcedureEval String [Expr]
    deriving Show
    
    