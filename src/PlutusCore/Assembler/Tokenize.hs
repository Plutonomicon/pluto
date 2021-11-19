{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module PlutusCore.Assembler.Tokenize
  ( tokenize
  , Token (..)
  , ErrorMessage (..)
  ) where


import Data.Either.Combinators (mapLeft)
import Data.Text (cons, pack)
import Data.Attoparsec.Text (Parser, parseOnly, endOfInput, many', choice, char, string, inClass, notInClass, satisfy, signed, decimal)

import PlutusCore.Assembler.Prelude
import PlutusCore.Assembler.Types.Builtin (Builtin (..))
import qualified PlutusCore.Assembler.Types.InfixBuiltin as Infix
import PlutusCore.Assembler.Types.Token (Token (..))


newtype ErrorMessage = ErrorMessage { getErrorMessage :: Text }


tokenize :: Text -> Either ErrorMessage [Token]
tokenize = mapLeft (ErrorMessage . pack) . parseOnly tokens


tokens :: Parser [Token]
tokens = do
  ts <- many' (many' whitespace >> token)
  endOfInput
  return ts


whitespace :: Parser ()
whitespace = void (satisfy (inClass " \t\r\n")) <|> comment


comment :: Parser ()
comment = do
  void $ string "--"
  void $ many' (satisfy (notInClass lineEnding))
  void $ satisfy (inClass lineEnding)
  where
    lineEnding = "\r\n"


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
var = do
  first <- satisfy (inClass ['a'..'z'])
  rest  <- many' (satisfy (inClass (['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "_")))
  return (Var (cons first (pack rest)))


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
integerLiteral = Integer <$> signed decimal


byteStringLiteral :: Parser Token
byteStringLiteral =
      hexadecimalByteStringLiteral
  <|> base64ByteStringLiteral


hexadecimalByteStringLiteral :: Parser Token
hexadecimalByteStringLiteral = todo


base64ByteStringLiteral :: Parser Token
base64ByteStringLiteral = todo


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
builtin =
  Builtin <$> choice
  [ AddInteger <$ string "AddInteger"
  , SubtractInteger <$ string "SubtractInteger"
  , MultiplyInteger <$ string "MultiplyInteger"
  , DivideInteger <$ string "DivideInteger"
  , QuotientInteger <$ string "QuotientInteger"
  , RemainderInteger <$ string "RemainderInteger"
  , ModInteger <$ string "ModInteger"
  , EqualsInteger <$ string "EqualsInteger"
  , LessThanInteger <$ string "LessThanInteger"
  , LessThanEqualsInteger <$ string "LessThanEqualsInteger"
  , AppendByteString <$ string "AppendByteString"
  , ConsByteString <$ string "ConsByteString"
  , SliceByteString <$ string "SliceByteString"
  , LengthByteString <$ string "LengthByteString"
  , IndexByteString <$ string "IndexByteString"
  , EqualsByteString <$ string "EqualsByteString"
  , LessThanByteString <$ string "LessThanByteString"
  , LessThanEqualByteString <$ string "LessThanEqualByteString"
  , Sha2_256 <$ string "Sha2_256"
  , Sha3_256 <$ string "Sha3_256"
  , Blake2b_256 <$ string "Blake2b_256"
  , VerifySignature <$ string "VerifySignature"
  , AppendString <$ string "AppendString"
  , EqualsString <$ string "EqualsString"
  , EncodeUtf8 <$ string "EncodeUtf8"
  , DecodeUtf8 <$ string "DecodeUtf8"
  , IfThenElse <$ string "IfThenElse"
  , ChooseUnit <$ string "ChooseUnit"
  , Trace <$ string "Trace"
  , FstPair <$ string "FstPair"
  , SndPair <$ string "SndPair"
  , ChooseList <$ string "ChooseList"
  , MkCons <$ string "MkCons"
  , HeadList <$ string "HeadList"
  , TailList <$ string "TailList"
  , NullList <$ string "NullList"
  , ChooseData <$ string "ChooseData"
  , ConstrData <$ string "ConstrData"
  , MapData <$ string "MapData"
  , ListData <$ string "ListData"
  , IData <$ string "IData"
  , BData <$ string "BData"
  , UnConstrData <$ string "UnConstrData"
  , UnMapData <$ string "UnMapData"
  , EqualsData <$ string "EqualsData"
  , MkPairData <$ string "MkPairData"
  , MkNilData <$ string "MkNilData"
  , MkNilPairData <$ string "MkNilPairData"
  ]


infixBuiltin :: Parser Token
infixBuiltin =
  InfixBuiltin <$> choice
  [ Infix.AddInteger <$ string "+i"
  , Infix.SubtractInteger <$ string "-i"
  , Infix.MultiplyInteger <$ string "*i"
  , Infix.DivideInteger <$ string "/i"
  , Infix.RemainderInteger <$ string "%i"
  , Infix.EqualsInteger <$ string "==i"
  , Infix.LessThanInteger <$ string "<i"
  , Infix.LessThanEqualsInteger <$ string "<=i"
  , Infix.AppendByteString <$ string "+b"
  , Infix.ConsByteString <$ string ":b"
  , Infix.IndexByteString <$ string "!b"
  , Infix.EqualsByteString <$ string "==b"
  , Infix.LessThanByteString <$ string "<b"
  , Infix.LessThanEqualByteString <$ string "<=b"
  , Infix.AppendString <$ string "+s"
  , Infix.EqualsString <$ string "==s"
  , Infix.EqualsData <$ string "==d"
  ]


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
