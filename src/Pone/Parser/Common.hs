
module Pone.Parser.Common
( parens
, brackets
, identifier
, lexeme
, whiteSpace
, reserved
, integer
, float
, comma
, stringLiteral
) where

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

reservedWords = [ "begin"
                , "end"
                , "match"
                , "with"
                , "in"
                , "interface"
                , "extends"
                , "type"
                , "abstract"
                , "as"
                , "is"
                , "implement"
                , "for"
                , "where"
                , "unknown"
                , "|"
                , "->"
                , "."
                , "λ"
                , "<"
                ]

lexer  =
  P.makeTokenParser $ emptyDef { P.reservedNames = reservedWords
                               , P.caseSensitive = True
                               , P.identStart = lower
                               , P.commentStart = ">--{"
                               , P.commentEnd = "}--<"
                               , P.commentLine = ";"
                               --, P.identLetter = alphaNum
                               }

parens        = P.parens lexer
brackets      = P.brackets lexer
identifier    = P.identifier lexer
lexeme        = P.lexeme lexer
whiteSpace    = P.whiteSpace lexer
reserved      = P.reserved lexer
integer       = P.integer lexer
float         = P.float lexer
comma         = P.comma lexer
stringLiteral = P.stringLiteral lexer
