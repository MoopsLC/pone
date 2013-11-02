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
import qualified Data.Map as Map

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

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

languageDef = emptyDef{ commentStart = "<"
                      , commentEnd = ">"
                      , identStart = letter
                      , identLetter = alphaNum
                      , opStart = oneOf "+*"
                      , opLetter = oneOf "+*"
                      , reservedOpNames = ["+", "*"]
                      , reservedNames = ["define", "procedure", "as", "in", "is"]
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
       <|> fmap Identifier m_identifier
       <|> do { m_reserved "define"
              ; name <- m_identifier
              ; m_reserved "as"
              ; value <- exprParser
              ; m_reserved "in"
              ; expr <- exprParser
              ; return $ Assign name value expr
              }
       <|> do { m_reserved "procedure"
              ; name <- m_identifier
              ; args <- argParser
              ; m_reserved "is"
              ; value <- exprParser
              ; m_reserved "in"
              ; expr <- exprParser
              ; return $ Procedure name args value expr
              }
       
spaceSep1 p = sepBy1 p m_whiteSpace
       
argParser :: Parser [String]
argParser = fmap (id) (spaceSep1 m_identifier)
       
       
mainParser :: Parser Expr
mainParser = m_whiteSpace >> exprParser <* eof
          
type Environment = Map.Map String Integer

pushName :: Environment -> Integer -> String -> Environment
pushName env value name = Map.insert name value env

lookupName :: Environment -> String -> Integer
lookupName env name = env Map.! name --fixme, handle unbound names

eval :: Environment -> Expr -> Integer
eval env (Assign name v e) = let value = eval env v
                                 newState = pushName env value name 
                              in eval newState e
eval env (Value i) = i
eval env (Binop op e1 e2) = let v1 = eval env e1
                                v2 = eval env e2
                             in case op of 
                                 Plus -> v1 + v2
                                 Times -> v1 * v2
eval env (Paren e) = eval env e
eval env (Identifier s) = lookupName env s
eval env (Procedure name args value expr) = eval env expr
   
-- change map to (implicit) stack of maps
main = do
    source <- readFile "C:/Users/M/Desktop/pone/pone_src/test.pone"
    case parse mainParser "" source of 
       Left err -> print err
       Right ans -> print $ eval (Map.empty) ans
    



       