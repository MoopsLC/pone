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

    <procedure-bind> ::= "define" <ident> <arg> (<arg> *) "as" <expr> in <expr>
           
    <binop> ::= "+" | "*"

    <ident> ::= [a-zA-Z][\w]*
    
    <procedure-eval> ::= <itent> (<args> +)

    <integer> ::= [0-9]+

-}


data Op = Plus | Times deriving Show
data Expr = Value Integer 
          | Binop Op Expr Expr
          | IdentifierBind String Expr Expr 
          | IdentifierEval String
          | ProcedureBind String [String] Expr Expr
          | ProcedureEval String [Expr]
    deriving Show
    
    