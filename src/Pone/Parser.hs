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
                      , commentLine = "comment"
                      , identStart = letter
                      , identLetter = alphaNum
                      , opStart = oneOf "+*"
                      , opLetter = oneOf "+*"
                      , reservedOpNames = ["+", "*"]
                      , reservedNames = ["define", "as", "in"]
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
       <|> do { number <- m_number
              ; return $ Value $ PoneInteger number
              }
       <|> try(do { name <- m_identifier
                  ; args <- argParser
                  ; return $ ProcedureEval name args
                  })
       <|> fmap IdentifierEval m_identifier
       
       <|> do { m_reserved "define"
              ; name <- m_identifier
              ; m_reserved "as"
              ; value <- exprParser
              ; m_reserved "in"
              ; expr <- exprParser
              ; return $ LocalIdentifierBind (IdentifierBind name value) expr
              }
        <|> do { m_reserved "define"
              ; name <- m_identifier
              ; params <- paramParser
              ; m_reserved "as"
              ; value <- exprParser
              ; m_reserved "in"
              ; expr <- exprParser
              ; return $ LocalProcedureBind (ProcedureBind name params value) expr
              }
       
       
--refactor to get these both from the same place
globalDefParser :: Parser GlobalDef
globalDefParser = try(do { m_reserved "define"
                         ; name <- m_identifier
                         ; params <- paramParser
                         ; m_reserved "as"
                         ; value <- exprParser 
                         ; return $ GlobalProcedureBind (ProcedureBind name params value)
                         })
                  <|> do { m_reserved "define"
                         ; name <- m_identifier
                         ; m_reserved "as"
                         ; value <- exprParser 
                         ; return $ GlobalIdentifierBind (IdentifierBind name value)
                         }
       
spaceSep1 p = sepBy1 p m_whiteSpace
       
paramParser :: Parser [String]
paramParser = spaceSep1 m_identifier
     
argParser :: Parser [Expr]
argParser = spaceSep1 exprParser
     
programParser :: Parser PoneProgram
programParser = do { globalDefs <- many globalDefParser
                   ; expr <- exprParser
                   ; return $ Program globalDefs expr
                   }

mainParser :: Parser PoneProgram
mainParser = m_whiteSpace >> programParser <* eof

convertError :: Either ParseError PoneProgram -> Either String PoneProgram
convertError (Left err) = Left $ show err
convertError (Right prog) = Right prog

parsePone :: String -> Either String PoneProgram
parsePone src = convertError $ parse mainParser "" src

