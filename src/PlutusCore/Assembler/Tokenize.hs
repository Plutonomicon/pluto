{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module PlutusCore.Assembler.Tokenize
  ( tokenize
  , Token (..)
  , ErrorMessage (..)
  ) where


import Data.Either.Combinators (mapLeft)
import Data.Text (pack)
import Data.Attoparsec.Text (Parser, parseOnly, endOfInput, many', choice, char, string)

import PlutusCore.Assembler.Prelude
import PlutusCore.Assembler.Types.Token (Token (..))


newtype ErrorMessage = ErrorMessage { getErrorMessage :: Text }


tokenize :: Text -> Either ErrorMessage [Token]
tokenize = mapLeft (ErrorMessage . pack) . parseOnly tokens


tokens :: Parser [Token]
tokens = do
  ts <- many' (whitespace >> token)
  endOfInput
  return ts


whitespace :: Parser ()
whitespace = todo


token :: Parser Token
token =
  choice
  [ lambda
  , arrow
  , forceKeyword
  , delayKeyword
  , openParen
  , closeParen
  , errorKeyword
  , integerLiteral
  , byteStringLiteral
  , textLiteral
  , boolLiteral
  , openBracket
  , closeBracket
  , comma
  , openBrace
  , closeBrace
  , dataKeyword
  , sigmaKeyword
  , equals
  , builtin
  , infixBuiltin
  , letKeyword
  , semicolon
  , inKeyword
  , ifKeyword
  , thenKeyword
  , elseKeyword
  -- WARNING: var must come last in order to disambiguate tokenizing wrt keywords!
  , var
  ]


var :: Parser Token
var = todo


lambda :: Parser Token
lambda = Lambda <$ char '\\'


arrow :: Parser Token
arrow = Arrow <$ string "->"


forceKeyword :: Parser Token
forceKeyword = Force <$ string "force"


delayKeyword :: Parser Token
delayKeyword = Delay <$ string "delay"


openParen :: Parser Token
openParen = OpenParen <$ char '('


closeParen :: Parser Token
closeParen = CloseParen <$ char ')'


errorKeyword :: Parser Token
errorKeyword = Error <$ string "error"


integerLiteral :: Parser Token
integerLiteral = todo


byteStringLiteral :: Parser Token
byteStringLiteral = todo


textLiteral :: Parser Token
textLiteral = todo


boolLiteral :: Parser Token
boolLiteral = (Bool True <$ string "true") <|> (Bool False <$ string "false")


openBracket :: Parser Token
openBracket = OpenBracket <$ char '['


closeBracket :: Parser Token
closeBracket = CloseBracket <$ char ']'


comma :: Parser Token
comma = Comma <$ char ','


openBrace :: Parser Token
openBrace = OpenBrace <$ char '{'


closeBrace :: Parser Token
closeBrace = CloseBrace <$ char '}'


dataKeyword :: Parser Token
dataKeyword = Data <$ string "data"


sigmaKeyword :: Parser Token
sigmaKeyword = Sigma <$ string "sigma"


equals :: Parser Token
equals = Equals <$ char '='


builtin :: Parser Token
builtin = todo


infixBuiltin :: Parser Token
infixBuiltin = todo


letKeyword :: Parser Token
letKeyword = Let <$ string "let"


semicolon :: Parser Token
semicolon = Semicolon <$ char ';'


inKeyword :: Parser Token
inKeyword = In <$ string "in"


ifKeyword :: Parser Token
ifKeyword = If <$ string "if"


thenKeyword :: Parser Token
thenKeyword = Then <$ string "then"


elseKeyword :: Parser Token
elseKeyword = Else <$ string "else"


todo :: a
todo = todo
