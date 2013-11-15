module Pone.Parser.Type where

import Data.List

import Control.Applicative hiding ((<|>), optional, many)
import Text.Parsec
import Text.Parsec.String
import Pone.Ast
import Pone.Parser.Common

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

parseTypeRep :: Parser TypeRep
parseTypeRep = ArrowTR <$> parseArrow

parseArrow :: Parser [TypeRep]
parseArrow = sepBy1 (parseApply) (reserved "->")

parseApply :: Parser TypeRep
parseApply = ApplyTR <$> many1 (NameTR <$> parseTypeName <|> parens parseTypeRep)

parseTypeCtor :: Parser String
parseTypeCtor = lexeme $ ((:) <$> upper <*> many alphaNum)

parseTypeName :: Parser String
parseTypeName = (identifier <|> parseTypeCtor)

extractError :: Show a => Either String a -> String
extractError (Left err) = err
extractError (Right s) = show s

convertError :: Either ParseError t -> Either String t
convertError (Left err) = Left $ show err
convertError (Right prog) = Right prog

testType :: String -> String
testType src = extractError $ convertError $ parse parseTypeRep "" src

toType :: TypeRep -> (Type Kind)
toType (ApplyTR ts) = foldr1 ProdT (map toType ts)
toType (ArrowTR ts) = foldr1 (\x acc -> ProdT (ProdT Arrow x) acc) (map toType ts)
toType (NameTR n) = TypeValue n UnknownK

parseType :: Parser (Type Kind)
parseType = toType <$> parseTypeRep