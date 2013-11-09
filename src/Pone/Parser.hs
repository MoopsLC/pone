module Pone.Parser (parsePone) where

import Control.Applicative((<*), (*>), (<*>), (<$>))
import Text.Parsec
import Text.Parsec.String
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
                      , reservedNames = ["define", "as", "in", ";", "|", "->"]
                      , caseSensitive = True
                      }
    
    
     
TokenParser{ parens = m_parens
           , integer = m_number
           , float = m_float
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , whiteSpace = m_whiteSpace } = makeTokenParser languageDef

       
parseInteger :: Parser Var
parseInteger = PoneInteger <$> m_number

parseFloat :: Parser Var
parseFloat = PoneFloat <$> m_float
       
tryParseMany :: Parser a -> Parser [a]
tryParseMany parser = try (spaceSep1 parser) <|> return []

makeBindEval :: String -> [String] -> Expr -> Expr -> Expr
makeBindEval name params value expr = 
    case params of
        [] -> LocalIdentifierBind (IdentifierBind name value) expr
        xs -> LocalIdentifierBind (IdentifierBind name (defToLambda xs value)) expr--CODE CLONE 1

--makeDefEval :: String -> [Expr] -> Expr
--makeDefEval name args = 
--    case args of 
--        [] -> IdentifierEval name
--        xs -> ProcedureEval name xs
        
makeGlobalDefBind :: String -> [String] -> Expr -> GlobalDef
makeGlobalDefBind name params value = 
    case params of
        [] -> GlobalIdentifierBind (IdentifierBind name value)
        xs -> GlobalIdentifierBind (IdentifierBind name (defToLambda xs value))--CODE CLONE 1
        
defToLambda :: [String] -> Expr -> Expr
defToLambda names expr = foldr (\x acc -> Value $ Lam $ Lambda x acc) expr names

parseLocalBind :: Parser Expr
parseLocalBind = 
    makeBindEval <$> (m_reserved "define" *> m_identifier)
                 <*> (tryParseMany m_identifier)
                 <*> (m_reserved "as" *> parseExpr)
                 <*> (m_reserved "in" *> parseExpr)
    
parseLambda :: Parser Var
parseLambda = 
    (Lam .: Lambda) <$> (m_identifier <* m_reserved "->")
                    <*> parseExpr

parsePatternMatch :: Parser Expr
parsePatternMatch = 
    PatternMatch <$> (m_reserved "match" *> parseExpr)
                 <*> (m_reserved "with" *> m_parens (many parsePattern))
                 
parsePattern :: Parser Pattern
parsePattern = 
    Pattern <$> (m_reserved "|" *> (UserType <$> typeIdentifier))
            <*> (m_reserved "->" *> parseExpr)
    
parseVar :: Parser Var
parseVar = (UserType <$> typeIdentifier)
       <|> parseInteger
       <|> try(parseFloat)
       <|> (Identifier <$> m_identifier)

parseExprNoApply :: Parser Expr
parseExprNoApply = (Value <$> parseVar)
               <|> parsePatternMatch
               <|> parseLocalBind
               <|> m_parens parseExpr
        
        
parseExpr :: Parser Expr
parseExpr = try(parseApply) <|> parseExpr

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

data ApplyList = ApplyList [Expr]

listToApply :: ApplyList -> Expr
listToApply (ApplyList xs) = foldl1 Apply xs

parseApply :: Parser Expr
parseApply = (listToApply . ApplyList) <$> do { first <- parseExprNoApply
                                              ; rest <- many parseExprNoApply
                                              ; return $ (first:rest)
                                              }



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
parsePone src = convertError $ printAst $ parse parseMain "" src


