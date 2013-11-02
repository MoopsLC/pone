module Pone.Parser (parsePone) where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

import Pone.Ast

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

parsePone :: String => Either ParseError Expr
parsePone src = parse mainParser "" src