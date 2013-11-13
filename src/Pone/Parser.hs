
module Pone.Parser (parsePone) where

import Control.Applicative((<*), (*>), (<*>), (<$>))
import Control.Monad
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Debug.Trace

import Pone.Ast
import Pone.Utils ((.:))

languageDef = emptyDef{ commentStart = "<"
                      , commentEnd = ">"
                      , commentLine = ";"
                      , identStart = lower
                      , identLetter = alphaNum
                      , reservedNames = ["begin", "end", "match", "with", "in", "interface", "extends", "type", "abstract", "as", "is", "implement", "for", "where", "unknown", "|", "->"]
                      , caseSensitive = True
                      }



TokenParser{ parens = m_parens
           , integer = m_number
           , float = m_float
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , comma = m_comma
           , whiteSpace = m_whiteSpace } = makeTokenParser languageDef

spaceSep1 :: Parser t -> Parser [t]
spaceSep1 p = sepBy1 p m_whiteSpace

comSep1 :: Parser t -> Parser [t]
comSep1 p = sepBy1 p m_comma

tryParseMany :: (Parser t -> Parser [t]) -> Parser t -> Parser [t]
tryParseMany sep parser = try (sep parser) <|> return []

tryParseManySpace :: Parser a -> Parser [a]
tryParseManySpace parser = tryParseMany spaceSep1 parser

tryParseManyComma :: Parser a -> Parser [a]
tryParseManyComma parser = tryParseMany comSep1 parser

tryParseMaybe :: Parser a -> Parser (Maybe a)
tryParseMaybe parser = (Just <$> try (parser)) <|> return Nothing

parseTypeId :: Parser String
parseTypeId = try $ do { x <- (upper <|> char '*')
                       ; xs <- many alphaNum
                       ; _ <- m_whiteSpace
                       ; return (x:xs)
                       }

getLoc :: Parser Location
getLoc = do
    pos <- getPosition
    return $ Location (sourceLine pos) (sourceColumn pos) (sourceName pos)

parseMain :: Parser (PoneProgram (Type Kind))
parseMain = Program <$> (many parseGlobalDef) <*> parseExpr

parseGlobalDef :: Parser (GlobalDef (Type Kind))
parseGlobalDef =
      DefSource <$> getLoc <*> choice [ parseTypeDef
                                      , parseInterfaceDef
                                      -- , parseGlobalFunction
                                      -- , parseImplementationDef
                                      ]

parseTypeDef :: Parser (GlobalDef t)
parseTypeDef =
    TypeDef <$> (m_reserved "type" *> parseTypeId)
            <*> (tryParseManySpace m_identifier)
            <*> (m_reserved "is" *> parseTypeDefList <* m_reserved "end")

parseTypeDefList :: Parser [TypeCtor]
parseTypeDefList =
    tryParseManySpace (m_reserved "|" *> parseTypeCtor)

parseTypeCtor :: Parser TypeCtor
parseTypeCtor = TypeCtor <$> parseTypeId <*> tryParseManySpace (parseTypeId <|> m_identifier)


parseInterfaceDef :: Parser (GlobalDef (Type Kind)) =
      InterfaceDef <$> (m_reserved "interface" *> parseTypeCtor)
                   <*> (try (m_reserved "extends" *> tryParseManyComma parseTypeCtor) <|> return [])
                   <*> (m_reserved "is" *> tryParseManySpace parseDefinition <* m_reserved "end")


parseDefinition :: Parser (Definition (Type Kind))
parseDefinition =
    Definition <$> (m_reserved "define" *> m_identifier)
               <*> tryParseManySpace m_identifier
               <*> (m_reserved ":" *> parseType)
               <*> (try (m_reserved "where" *> m_parens (tryParseManyComma parseConstraint)) <|> return [])
               <*> (    (m_reserved "as" *> (Just <$> parseExpr) <* try(m_reserved "end"))
                    <|> (m_reserved "abstract" *> return Nothing))


parseConstraint :: Parser (Constraint (Type Kind))
parseConstraint =
    Constraint <$> m_identifier
               <*> (m_reserved "<" *> parseType)

parseTypeNoArrow :: Parser (Type Kind)
parseTypeNoArrow = m_parens parseType
                 <|> (TypeValue <$> (parseTypeId <|> m_identifier) <*> return UnknownK)

parseType :: Parser (Type Kind)
parseType = try(parseTypeArrow) <|> parseTypeNoArrow

parseTypeArrow :: Parser (Type Kind)
parseTypeArrow =
    (listToArrow .: ArrowList) <$> (parseTypeNoArrow)
                               <*> many (m_reserved "->" *> parseTypeNoArrow)


parseImplementationDef :: Parser (GlobalDef (Type Kind))
parseImplementationDef =
    ImplementationDef <$> (m_reserved "implement" *> parseTypeId)
                      <*> (m_reserved "for" *> parseTypeId)
                      <*> (m_reserved "as" *> tryParseManySpace parseDefinition <* m_reserved "end")

parseGlobalFunction :: Parser (GlobalDef (Type Kind))
parseGlobalFunction =
    GlobalFunction <$> (parseDefinition <* m_reserved "end")

parseExpr :: Parser (Expr (Type Kind))
parseExpr = {-Source <$> getLoc <*> -}(Literal <$> parseInteger <*> return bottom)

parseInteger :: Parser Value
parseInteger = PoneInteger <$> m_number

convertError :: Either ParseError (PoneProgram t) -> Either String (PoneProgram t)
convertError (Left err) = Left $ show err
convertError (Right prog) = Right prog

printAst :: Show b => Either a b -> Either a b
printAst arg = case arg of
    Left err -> arg
    Right ast -> trace (show ast) arg

parsePone :: String -> Either String (PoneProgram (Type Kind))
parsePone src = convertError $ parse parseMain "" src

--parseProgram :: Parser PoneProgram
--parseProgram = Program <$> (many parseGlobalDef) <*> parseExpr

--parseMain :: Parser PoneProgram
--parseMain = m_whiteSpace *> parseProgram <* eof

--languageDef = emptyDef{ commentStart = "<"
--                      , commentEnd = ">"
--                      , commentLine = "comment"
--                      , identStart = lower
--                      , identLetter = alphaNum
--                      , reservedNames = ["define", "as", "in", ";", "|", "->"]
--                      , caseSensitive = True
--                      }



--TokenParser{ parens = m_parens
--           , integer = m_number
--           , float = m_float
--           , identifier = m_identifier
--           , reservedOp = m_reservedOp
--           , reserved = m_reserved
--           , whiteSpace = m_whiteSpace } = makeTokenParser languageDef


--parseInteger :: Parser Var
--parseInteger = PoneInteger <$> m_number

--parseFloat :: Parser Var
--parseFloat = PoneFloat <$> m_float

--tryParseMany :: Parser a -> Parser [a]
--tryParseMany parser = try (spaceSep1 parser) <|> return []

--makeId :: String -> [String] -> Expr -> IdentifierBind
--makeId name params value =
--    case params of
--        [] -> IdentifierBind name value
--        xs -> IdentifierBind name (defToLambda xs value)

--makeLocalBind :: String -> [String] -> Expr -> Expr -> Expr
--makeLocalBind name params value expr =
--      LocalIdentifierBind (makeId name params value) expr

--makeGlobalDefBind :: String -> [String] -> Expr -> GlobalDef
--makeGlobalDefBind name params value =
--    GlobalIdentifierBind (makeId name params value)

--defToLambda :: [String] -> Expr -> Expr
--defToLambda names expr = foldr (\x acc -> Value $ Lam $ Lambda x acc) expr names

--parseLocalBind :: Parser Expr
--parseLocalBind =
--    makeLocalBind <$> (m_reserved "define" *> m_identifier)
--                 <*> (tryParseMany m_identifier)
--                 <*> (m_reserved "as" *> parseExpr)
--                 <*> (m_reserved "in" *> parseExpr)

--parseLambda :: Parser Var
--parseLambda =
--    (Lam .: Lambda) <$> (m_identifier <* m_reserved "->")
--                    <*> parseExpr

--parsePatternMatch :: Parser Expr
--parsePatternMatch =
--    PatternMatch <$> (m_reserved "match" *> parseExpr)
--                 <*> (m_reserved "with" *> m_parens (many parsePattern))

--parsePattern :: Parser Pattern
--parsePattern =
--    Pattern <$> (m_reserved "|" *> (UserType <$> typeIdentifier))
--            <*> (m_reserved "->" *> parseExpr)

--parseVar :: Parser Var
--parseVar = (UserType <$> typeIdentifier)
--       <|> try(parseFloat)
--       <|> parseInteger
--       <|> try(parseLambda)
--       <|> (Identifier <$> m_identifier)

--parseExprNoApply :: Parser Expr
--parseExprNoApply = parsePatternMatch
--               <|> (Value <$> parseVar)
--               <|> parseLocalBind
--               <|> m_parens parseExpr

--data ApplyList = ApplyList [Expr]

--listToApply :: ApplyList -> Expr
--listToApply (ApplyList xs) = foldl1 Apply xs

--parseApply :: Parser Expr
--parseApply = (listToApply . ApplyList) <$> do { first <- parseExprNoApply
--                                              ; second <- parseExprNoApply
--                                              ; rest <- many parseExprNoApply
--                                              ; return $ (first:second:rest)
--                                              }

--parseExpr :: Parser Expr
--parseExpr = try(parseApply) <|> parseExprNoApply

--parseTypeBind :: Parser GlobalDef
--parseTypeBind = (GlobalTypeBind .: TypeBind)
--    <$> (m_reserved "type" *> typeIdentifier)
--    <*> (m_reserved "is" *> (rodSep1 typeIdentifier) <* m_reserved ";")


--parseFunctionBind :: Parser GlobalDef
--parseFunctionBind =
--    makeGlobalDefBind <$> (m_reserved "define" *> m_identifier)
--                      <*> (try (many m_identifier) <|> return [])
--                      <*> (m_reserved "as" *> parseExpr <* m_reserved ";")

--parseGlobalDef :: Parser GlobalDef
--parseGlobalDef = try(parseFunctionBind)
--             <|> parseTypeBind


--typeIdentifier :: Parser String
--typeIdentifier = try $ do { x <- upper
--                          ; xs <- many alphaNum
--                          ; _ <- m_whiteSpace
--                          ; return (x:xs)
--                          }


--rodSep1 p = sepBy1 p (m_reserved "|")

--spaceSep1 p = sepBy1 p m_whiteSpace

--parseProgram :: Parser PoneProgram
--parseProgram = Program <$> (many parseGlobalDef) <*> parseExpr

--parseMain :: Parser PoneProgram
--parseMain = m_whiteSpace *> parseProgram <* eof

--convertError :: Either ParseError PoneProgram -> Either String PoneProgram
--convertError (Left err) = Left $ show err
--convertError (Right prog) = Right prog

--printAst :: Either a PoneProgram -> Either a PoneProgram
--printAst arg = case arg of
--    Left err -> arg
--    Right ast -> trace (show ast) arg

--parsePone :: String -> Either String PoneProgram
--parsePone src = convertError $ {-printAst $-} parse parseMain "" src


