{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
{-
Grammar:


<program> ::= (<arg> *)

<arg> ::= identifier

-}



data Program = Args String [String] deriving Show
languageDef = emptyDef{ identStart = letter
                      , identLetter = alphaNum
                      }
    
TokenParser{ identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , whiteSpace = m_whiteSpace } = makeTokenParser languageDef
           
           
spaceSep1 p = sepBy1 p m_whiteSpace
       
argParser :: Parser [String]
argParser = fmap (id) (spaceSep1 m_identifier)

programParser :: Parser Program
programParser = do { name <- m_identifier
                   ; args <- argParser
                   ; return $ Args name args
                   }

mainParser = m_whiteSpace >> programParser <* eof

main =
    let source = "name arg1 arg2 arg3" in
    case parse mainParser "" source of 
       Left err -> print err
       Right ans -> print ans