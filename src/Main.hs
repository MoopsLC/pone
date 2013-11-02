{-# LANGUAGE NoMonomorphismRestriction #-}

import Pone.Tree
import Pone.Foldable
import Pone.List
import Pone.PFunctor
import Pone.Applicative
import Pone.Option
import System.IO
import Data.Monoid
import Pone.RandomStuff
    

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

{-
Grammar:

<expr>    ::= <letexpr>
            | "(" <expr> ")"
            | <expr> <binop> <expr>
            | <ident>
            | <integer>
           
<letexpr> ::= "let" <ident> "be" <expr> "in" <expr>
            
<binop>   ::= "+" | "*"

<ident>   ::= [a-zA-Z][\w]*

<integer> ::= [0-9]+

-}


data Op = Plus | Times deriving Show
data Expr = Assign String Expr Expr | Value Integer | Binop Op Expr Expr | Paren Expr | Identifer String deriving Show

languageDef = emptyDef{ commentStart = "<"
                      , commentEnd = ">"
                      , identStart = letter
                      , identLetter = alphaNum
                      , opStart = oneOf "+*"
                      , opLetter = oneOf "+*"
                      , reservedOpNames = ["+", "*"]
                      , reservedNames = ["let", "be", "in"]
                      }
    
TokenParser{ parens = m_parens
           , integer = m_number
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , whiteSpace = m_whiteSpace } = makeTokenParser languageDef
        
opTable = [ [Infix (m_reservedOp "+" >> return (Binop Plus)) AssocLeft]
          , [Infix (m_reservedOp "*" >> return (Binop Times)) AssocLeft]
          ]
        
exprParser :: Parser Expr
exprParser = buildExpressionParser opTable term <?> "expression"
    
term = m_parens exprParser
       <|> fmap Value m_number
       <|> fmap Identifer m_identifier
       <|> do { m_reserved "let"
              ; name <- m_identifier
              ; m_reserved "be"
              ; value <- exprParser
              ; m_reserved "in"
              ; expr <- exprParser
              ; return $ Assign name value expr
              }
       
mainParser :: Parser Expr
mainParser = m_whiteSpace >> exprParser <* eof
    
input = "let a be (1 + (2 * 4)) in a" 

main = case parse mainParser "" input of 
       Left err -> print err
       Right ans -> print ans
    
