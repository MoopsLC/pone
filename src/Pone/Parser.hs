module Pone.Parser (parsePone) where

import Control.Applicative((<*), (*>), (<*>), (<$>))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Debug.Trace

import Pone.Ast

               
(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = ((.) . (.))

languageDef = emptyDef{ commentStart = "<"
                      , commentEnd = ">"
                      , commentLine = "comment"
                      , identStart = lower
                      , identLetter = alphaNum
                      , reservedNames = ["define", "as", "in", ";", "|"]
                      , caseSensitive = True
                      }
    
    
    
TokenParser{ parens = m_parens
           , integer = m_number
           , float = m_float
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , whiteSpace = m_whiteSpace } = makeTokenParser languageDef

       
parseInteger :: Parser Expr
parseInteger = (Value . PoneInteger) <$> m_number

parseFloat :: Parser Expr
parseFloat = (Value . PoneFloat) <$> m_float
       
tryParseMany :: Parser a -> Parser [a]
tryParseMany parser = try (spaceSep1 parser) <|> return []

makeBindEval :: String -> [String] -> Expr -> Expr -> Expr
makeBindEval name params value expr = 
    case params of
        [] -> LocalIdentifierBind (IdentifierBind name value) expr
        xs -> LocalProcedureBind (ProcedureBind name xs value) expr

makeDefEval :: String -> [Expr] -> Expr
makeDefEval name args = 
    case args of 
        [] -> IdentifierEval name
        xs -> ProcedureEval name xs
        
makeGlobalDefBind :: String -> [String] -> Expr -> GlobalDef
makeGlobalDefBind name params value = 
    case params of
        [] -> GlobalIdentifierBind (IdentifierBind name value)
        xs -> GlobalProcedureBind (ProcedureBind name xs value)
        
parseLocalBind :: Parser Expr
parseLocalBind = 
    makeBindEval <$> (m_reserved "define" *> m_identifier)
                 <*> (tryParseMany m_identifier)
                 <*> (m_reserved "as" *> parseExpr)
                 <*> (m_reserved "in" *> parseExpr)


parseDefEval :: Parser Expr
parseDefEval = 
    makeDefEval <$> m_identifier
                <*> tryParseMany parseExpr
    
parseExpr :: Parser Expr
parseExpr = m_parens parseExpr
        <|> try(parseFloat)
        <|> parseInteger
        <|> parseDefEval
        <|> parseLocalBind
        <|> ((Value . UserType) <$> typeIdentifier)

-- (..:) :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
-- (..:) = ((.) . (.)) . (.)

-- (...:) :: (e -> f) -> (a -> b -> c -> d -> e) -> (a -> b -> c -> d -> f)
-- (...:) = (((.) . (.)) . (.)) . (.)

parseTypeBind :: Parser GlobalDef
parseTypeBind = (GlobalTypeBind .: TypeBind)
    <$> (m_reserved "type" *> typeIdentifier) 
    <*> (m_reserved "is" *> (rodSep1 typeIdentifier) <* m_reserved ";")
     

parseFunctionBind :: Parser GlobalDef
parseFunctionBind = 
    makeGlobalDefBind <$> (m_reserved "define" *> m_identifier)
                      <*> (try (many m_identifier) <|> return [])
                      <*> (m_reserved "as" *> parseExpr <* m_reserved ";")

parseGlobalDef :: Parser GlobalDef
parseGlobalDef = try(parseFunctionBind)
             <|> parseTypeBind

                  
typeIdentifier :: Parser String
typeIdentifier = try $ do { x <- upper
                          ; xs <- many alphaNum
                          ; _ <- m_whiteSpace
                          ; return (x:xs)
                          }


rodSep1 p = sepBy1 p (m_reserved "|")
    
spaceSep1 p = sepBy1 p m_whiteSpace
     
parseProgram :: Parser PoneProgram
parseProgram = Program <$> (many parseGlobalDef) <*> parseExpr

parseMain :: Parser PoneProgram
parseMain = m_whiteSpace *> parseProgram <* eof

convertError :: Either ParseError PoneProgram -> Either String PoneProgram
convertError (Left err) = Left $ show err
convertError (Right prog) = Right prog

printAst :: Either a PoneProgram -> Either a PoneProgram
printAst arg = case arg of 
    Left err -> arg
    Right ast -> trace (show ast) arg

parsePone :: String -> Either String PoneProgram
parsePone src = convertError $ {-printAst $-} parse parseMain "" src


