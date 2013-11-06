module Pone.Parser (parsePone) where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

import Debug.Trace

import Pone.Ast

languageDef = emptyDef{ commentStart = "<"
                      , commentEnd = ">"
                      , commentLine = "comment"
                      , identStart = letter
                      , identLetter = alphaNum
                      , reservedNames = ["define", "as", "in", ";"]
                      }
    
TokenParser{ parens = m_parens
           , integer = m_number
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , whiteSpace = m_whiteSpace } = makeTokenParser languageDef
        

        
--exprParser :: Parser Expr
--exprParser = buildExpressionParser opTable term <?> "expression"
    
exprParser = m_parens exprParser
       -- <|> fmap IdentifierEval m_identifier
        <|> do { number <- m_number
               ; return $ Value $ PoneInteger number
               }
        <|> do { name <- m_identifier
               ; args <- try (spaceSep1 exprParser) <|> return []
               ; return $ case args of 
                   [] -> IdentifierEval name
                   xs -> ProcedureEval name args
               }
        <|> do { m_reserved "define"
               ; name <- m_identifier
               ; params <- try (many m_identifier) <|> return []
               ; m_reserved "as"
               ; value <- exprParser
               ; m_reserved "in"
               ; expr <- exprParser
               ; return $ case params of
                   [] -> LocalIdentifierBind (IdentifierBind name value) expr
                   xs -> LocalProcedureBind (ProcedureBind name xs value) expr
               }
        
       
globalDefParser :: Parser GlobalDef
                  --fixme, this usually leads to the whole program being parsed twice
globalDefParser = try(do { m_reserved "define"
                         ; name <- m_identifier
                         ; params <- try (many m_identifier) <|> return []
                         ; m_reserved "as"
                         ; value <- exprParser 
                         ; m_reserved ";"
                         ; return $ case params of
                             [] -> GlobalIdentifierBind (IdentifierBind name value)
                             xs -> GlobalProcedureBind (ProcedureBind name xs value)
                         })
       
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

printAst :: Either a PoneProgram -> Either a PoneProgram
printAst arg = case arg of 
    Left err -> arg
    Right ast -> trace (show ast) arg

parsePone :: String -> Either String PoneProgram
parsePone src = convertError $ {-printAst $-} parse mainParser "" src


