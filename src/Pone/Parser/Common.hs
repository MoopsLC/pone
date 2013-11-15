
module Pone.Parser.Common where

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

--languageDef = emptyDef{

--                      , identLetter = alphaNum
--                      , reservedNames =
--                      , caseSensitive = True
--                      }


lexer  = --undefined
  P.makeTokenParser $ emptyDef {P.reservedNames = [ "begin"
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
                                                  , "^" ]
                                      , P.caseSensitive = True
                                      , P.identStart = lower
                                      , P.commentStart = "&:"
                                      , P.commentEnd = ":&"
                                      , P.commentLine = ";"
                                      }

parens     = P.parens lexer
brackets   = P.brackets lexer
identifier = P.identifier lexer
lexeme     = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
reserved   = P.reserved lexer
integer    = P.integer lexer
float      = P.float lexer
comma      = P.comma lexer

--TokenParser { comma = m_comma
--            , whiteSpace = m_whiteSpace } = makeTokenParser languageDef
