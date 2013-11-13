
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
                      , reservedNames = ["begin", "end", "match", "with", "in", "interface", "extends", "type", "abstract", "as", "is", "implement", "for", "where", "unknown", "|", "->", ".", "λ"]
                      , caseSensitive = True
                      }



TokenParser { parens = m_parens
            , brackets = m_brackets
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
                                      , parseGlobalFunction
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
               <*> (    (m_reserved "as" *> (Just <$> parseExpr))
                    <|> (m_reserved "abstract" *> return Nothing))


parseConstraint :: Parser (Constraint (Type Kind))
parseConstraint =
    Constraint <$> m_identifier
               <*> (m_reserved "<" *> parseType)

parseTypeNoArrow :: Parser (Type Kind)
parseTypeNoArrow = m_parens parseType
                 <|> (TypeValue <$> (parseTypeId <|> m_identifier) <*> return UnknownK)

parseType :: Parser (Type Kind)
parseType = try(parseTypeArrow) <|> try(parseTypeApply) <|> parseTypeNoArrow

parseTypeNoApply :: Parser (Type Kind)
parseTypeNoApply = TypeValue <$> (m_identifier <|> parseTypeId) <*> return UnknownK

parseTypeArrow :: Parser (Type Kind)
parseTypeArrow =
    (listToArrow .: ArrowList) <$> (parseTypeNoArrow)
                               <*> many (m_reserved "->" *> parseTypeNoArrow)

parseTypeApply :: Parser (Type Kind)
parseTypeApply = parseGenApply ProdT parseTypeNoApply

parseImplementationDef :: Parser (GlobalDef (Type Kind))
parseImplementationDef =
    ImplementationDef <$> (m_reserved "implement" *> parseTypeId)
                      <*> (m_reserved "for" *> parseTypeId)
                      <*> (m_reserved "as" *> tryParseManySpace parseDefinition <* m_reserved "end")

parseGlobalFunction :: Parser (GlobalDef (Type Kind))
parseGlobalFunction =
    GlobalFunction <$> (parseDefinition <* m_reserved "end")

parseExprNoApply :: Parser (Expr (Type Kind))
parseExprNoApply =
    Source <$> getLoc <*> choice [ m_parens parseExpr
                                 , parseLiteral
                                 , m_brackets parseLambda
                                 , parseIdentifier]

parseExpr :: Parser (Expr (Type Kind))
parseExpr = try(parseApply) <|> parseExprNoApply

parseGenApply :: (t -> t -> t) -> Parser t -> Parser t
parseGenApply f parser =
    ((listToApply f) .: ApplyList) <$> parser <*> do { second <- parser
                                                     ; rest <- many parser
                                                     ; return $ (second:rest)
                                                     }
parseApply :: Parser (Expr (Type Kind))
parseApply = parseGenApply Apply parseExprNoApply

parseLiteral :: Parser (Expr (Type Kind))
parseLiteral = Literal <$> choice [ parseInteger
                                  ] <*> return UnknownT

parseIdentifier :: Parser (Expr (Type Kind))
parseIdentifier = Identifier <$> m_identifier <*> return UnknownT

parseLambda :: Parser (Expr (Type Kind))
parseLambda = Lambda <$> (m_reserved "λ" *> m_identifier)
                     <*> (m_reserved "." *> parseExpr)

parseInteger :: Parser Value
parseInteger = PoneInteger <$> m_number

convertError :: Either ParseError (PoneProgram t) -> Either String (PoneProgram t)
convertError (Left err) = Left $ show err
convertError (Right prog) = Right prog

printAst :: Show b => Either a b -> Either a b
printAst arg = case arg of
    Left err -> arg
    Right ast -> trace (show ast) arg

parsePone :: String -> String -> Either String (PoneProgram (Type Kind))
parsePone filename src = convertError $ parse parseMain {-filename {-removed until i get pretty printing working-}-} "" src
