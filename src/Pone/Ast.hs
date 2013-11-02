module Pone.Ast where

{-

  Grammar:


    <expr>    ::= <definition-expr>
                | <procedure-expr>
                | "(" <expr> ")"
                | <expr> <binop> <expr>
                | <procedure>
                | <ident>
                | <integer>
                
               
    <definition-expr> ::= "define" <ident> "as" <expr> "in" <expr>

    <procedure> ::= <ident> <arg> (<arg> *) "is" <expr>
           
    <binop> ::= "+" | "*"

    <ident> ::= [a-zA-Z][\w]*

    <integer> ::= [0-9]+

-}


data Op = Plus | Times deriving Show
data Expr = Assign String Expr Expr 
          | Value Integer 
          | Binop Op Expr Expr 
          | Paren Expr 
          | Identifier String
          | Procedure String [String] Expr Expr
    deriving Show
    
    