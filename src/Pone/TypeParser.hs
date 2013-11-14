
module Pone.TypeParser where

import Data.List
import Control.Applicative((<*), (*>), (<*>), (<$>))
import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Pone.Utils ((.:), printInline)


languageDef = emptyDef{ commentStart = "<"
                      , commentEnd = ">"
                      , commentLine = ";"
                      , identStart = lower
                      , identLetter = alphaNum
                      , reservedNames = ["->", ":", "as", ")"]
                      , caseSensitive = True
                      }



TokenParser { parens = m_parens
            , brackets = m_brackets
            , integer = m_number
            , float = m_float
            , symbol = m_symbol
            , identifier = m_identifier
            , reservedOp = m_reservedOp
            , reserved = m_reserved
            , comma = m_comma
            , whiteSpace = m_whiteSpace } = makeTokenParser languageDef

data Program = Program Type

instance Show Program where
    show (Program t) = ": " ++ (show t) ++ " as"

data Type = Apply [Type]
          | Arrow [Type]
          | Name String


instance Show Type where
    show (Name s) = s
    show (Apply ts) = "" ++ (unwords (map show ts)) ++ ""
    show (Arrow ts) = "(" ++ (showSpaced "->" ts) ++ ")"


showSpaced :: Show a => String -> [a] -> String
showSpaced str xs = unwords $ intersperse str (map show xs)

parseType :: Parser Type
parseType = (try parseArrow <?> "arrow")
        <|> (try parseApply <?> "apply")
        <|> parseNameOrType

parseTypeId :: Parser Type
parseTypeId = Name <$> parseTypeName

parseApply :: Parser Type
parseApply = let look = lookAhead (m_reserved "as" <|> m_reserved "->" <|> m_reserved ")") in
  let parseInner = m_parens parseType <|> parseTypeId in do
    first <- parseInner
    second <- parseType
    rest <- try (manyTill parseType look) <|> return []
    return $ Apply (first:second:rest)

parseArrow :: Parser Type
parseArrow = let look = lookAhead (m_reserved "as" <|> m_reserved ")") in
  let parseInner = m_parens parseType <|> parseTypeId in do
    first <- parseInner
    second <- m_reserved "->" *> parseType
    rest <- try (manyTill (m_reserved "->" *> parseType) look) <|> return []
    return $ Arrow (first:second:rest)

parseNameOrType :: Parser Type
parseNameOrType = (m_parens parseType <|> parseTypeId)

parseTypeName :: Parser String
parseTypeName = (m_identifier <|> parseTypeCtor)

parseTypeCtor :: Parser String
parseTypeCtor = try $ do { x <- (upper <|> char '*')
                         ; xs <- many alphaNum
                         ; _ <- m_whiteSpace
                         ; return (x:xs)
                         }

parseMain :: Parser Program
parseMain = m_whiteSpace *> m_reserved ":" *> (Program <$> parseType) <* m_reserved "as" <* eof

convertError :: Either ParseError t -> Either String t
convertError (Left err) = Left $ show err
convertError (Right prog) = Right prog

parsePoneType :: String -> Either String Program
parsePoneType src = printInline src $ convertError $ parse parseMain {-filename {-removed until i get pretty printing working-}-} "" src