module Pone.Parser.Type where

import Data.List

import Control.Applicative hiding ((<|>), optional, many)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

lexer  = P.makeTokenParser $ emptyDef {P.reservedNames = ["as", "->"]}
parens = P.parens lexer
identifier = P.identifier lexer
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
reserved = P.reserved lexer

data TypeRep = ApplyTR [TypeRep]
             | ArrowTR [TypeRep]
             | NameTR String

data Kind = ProdK Kind Kind
          | Star
          | UnknownK
    deriving (Show)

instance Show TypeRep where
    show (NameTR s) = s
    show (ApplyTR ts) = "" ++ (unwords (map show ts)) ++ ""
    show (ArrowTR ts) = "(" ++ (showSpaced "->" ts) ++ ")"

showSpaced :: Show a => String -> [a] -> String
showSpaced str xs = unwords $ intersperse str (map show xs)

reduce :: TypeRep -> TypeRep
reduce (NameTR string) = NameTR string
reduce (ApplyTR xs) = if (length xs == 1)
    then head xs
    else ApplyTR xs
reduce (ArrowTR xs) = if (length xs == 1)
    then head xs
    else ArrowTR xs


parseType :: Parser TypeRep
parseType = ArrowTR <$> parseArrow

parseArrow :: Parser [TypeRep]
parseArrow = sepBy1 (parseApply) (reserved "->")

parseApply :: Parser TypeRep
parseApply = ApplyTR <$> many1 (parseTypeName <|> parens parseType)

parseTypeCtor :: Parser String
parseTypeCtor = lexeme $ ((:) <$> upper <*> many alphaNum)

parseTypeName :: Parser TypeRep
parseTypeName = NameTR <$> (identifier <|> parseTypeCtor)

extractError :: Show a => Either String a -> String
extractError (Left err) = err
extractError (Right s) = show s

--parseMain :: Parser Type
--parseMain = liftA toType parseType

--toType :: TypeRep -> Type
--toType = undefined

convertError :: Either ParseError t -> Either String t
convertError (Left err) = Left $ show err
convertError (Right prog) = Right prog

parsePoneType :: String -> String
parsePoneType src = extractError $ convertError $ parse parseType "" src

--module Pone.TypeParser where

--import Data.List
--import Control.Applicative((<*), (*>), (<*>), (<$>))
--import Control.Monad
--import Text.Parsec
--import Text.Parsec.String
--import Text.Parsec.Token
--import Text.Parsec.Language
--import Pone.Utils ((.:), printInline)

--languageDef = emptyDef{ commentStart = "<"
--                      , commentEnd = ">"
--                      , commentLine = ";"
--                      , identStart = lower
--                      , identLetter = alphaNum
--                      , reservedNames = ["->", ":", "as", ")"]
--                      , caseSensitive = True
--                      }



--TokenParser { parens = m_parens
--            , brackets = m_brackets
--            , integer = m_number
--            , float = m_float
--            , symbol = m_symbol
--            , identifier = m_identifier
--            , reservedOp = m_reservedOp
--            , reserved = m_reserved
--            , comma = m_comma
--            , whiteSpace = m_whiteSpace } = makeTokenParser languageDef

--data Program = Program Type

--instance Show Program where
--    show (Program t) = ": " ++ (show t) ++ " as"

--data Type = Apply [Type]
--          | Arrow [Type]
--          | Name String


--instance Show Type where
--    show (Name s) = s
--    show (Apply ts) = "" ++ (unwords (map show ts)) ++ ""
--    show (Arrow ts) = "(" ++ (showSpaced "->" ts) ++ ")"


--showSpaced :: Show a => String -> [a] -> String
--showSpaced str xs = unwords $ intersperse str (map show xs)




--parseType :: Parser Type
--parseType = try parseArrow
--        <|> try parseApply
--        <|> m_parens parseType
--        <|> parseTypeId

--parseTypeId :: Parser Type
--parseTypeId = Name <$> parseTypeName

--parseApply :: Parser Type
--parseApply =
--  let ahead = lookAhead (m_reserved "as" <|> m_reserved "->" <|> m_reserved ")")
--      parseInner = parseType
--  in do
--      first <- (m_parens parseType <|> parseTypeId)
--      second <- parseInner
--      rest <- try (manyTill parseInner ahead) <|> return []
--      return $ Apply (first:second:rest)

--parseArrow :: Parser Type
--parseArrow =
--  let ahead = lookAhead (m_reserved "as" <|> m_reserved ")")
--      parseInner =  try parseApply <|> m_parens parseType <|> parseTypeId
--  in do
--      first <- (m_parens parseType <|> parseTypeId)
--      second <- m_reserved "->" *> parseInner
--      rest <- try (manyTill (m_reserved "->" *> parseInner) ahead) <|> return []
--      return $ Arrow (first:second:rest)

--parseTypeName :: Parser String
--parseTypeName = (m_identifier <|> parseTypeCtor)

--parseTypeCtor :: Parser String
--parseTypeCtor = try $ do { x <- (upper <|> char '*')
--                         ; xs <- many alphaNum
--                         ; _ <- m_whiteSpace
--                         ; return (x:xs)
--                         }

--parseMain :: Parser Program
--parseMain = m_whiteSpace *> (m_reserved ":") *> (Program <$> parseType) <* m_reserved "as" <* eof

--convertError :: Either ParseError t -> Either String t
--convertError (Left err) = Left $ show err
--convertError (Right prog) = Right prog

--parsePoneType :: String -> Either String Program
--parsePoneType src = convertError $ parse parseMain "" src