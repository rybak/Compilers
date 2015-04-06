module Lexer
       (identifier,
        reserved,

import Prelude
import Control.Applicative hiding ((<|>), many)
import Text.Parsec.Combinator
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

languageDef :: LanguageDef st
languageDef = emptyDef
              { commentStart   = "/*"
              , commentEnd     = "*/"
              , commentLine    = "//"
              , identStart     = letter
              , identLetter	 = alphaNum <|> oneOf "_'"
              , opStart	 = opLetter languageDef
              , opLetter	 = oneOf "+-/*<>=!"
              , reservedNames  = [ "function"
                                 , "skip"
                                 , "while", "do"
                                 , "end"
                                 , "if", "then", "else", "fi"
                                 , "true", "false"
                                 , "read", "write"
                                 ]
              , reservedOpNames= ["+", "-", "*", "/", "%"
                                 ,">", "<", "<=", ">="
                                 , "==", "!="
                                 , "and", "or", "not"]
              , caseSensitive  = True
              }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

