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

convertError :: Either ParseError t -> Either String t
convertError (Left err) = Left $ show err
convertError (Right prog) = Right prog

testType :: String -> String
testType src = extractError $ convertError $ parse parseType "" src
