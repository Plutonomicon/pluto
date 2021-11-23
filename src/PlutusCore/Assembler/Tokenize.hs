{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module PlutusCore.Assembler.Tokenize
  ( tokenize
  , printToken
  , printBuiltin
  , printInfixBuiltin
  , escapeText
  , Token (..)
  , ErrorMessage (..)
  ) where


import           Data.Attoparsec.Text                    (Parser, anyChar, char,
                                                          choice, decimal,
                                                          endOfInput, inClass,
                                                          many', notInClass,
                                                          parseOnly, satisfy,
                                                          signed, string)
import qualified Data.ByteString                         as BS
import           Data.Either.Combinators                 (mapLeft)
import           Data.Text                               (cons, pack, replace)
import           Data.Word                               (Word8)
import           Text.Hex                                (encodeHex)

import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Types.Builtin      (Builtin (..))
import qualified PlutusCore.Assembler.Types.InfixBuiltin as Infix
import           PlutusCore.Assembler.Types.Token        (Token (..))


newtype ErrorMessage = ErrorMessage { getErrorMessage :: Text }
  deriving (Eq, Show)


tokenize :: Text -> Either ErrorMessage [Token]
tokenize = mapLeft (ErrorMessage . pack) . parseOnly tokens


tokens :: Parser [Token]
tokens = do
  ts <- many' (many' whitespace >> token)
  many' whitespace >> endOfInput
  return ts


whitespace :: Parser ()
whitespace = void (satisfy (inClass " \t\r\n")) <|> oneLineComment <|> multiLineComment


oneLineComment :: Parser ()
oneLineComment = do
  void $ string "--"
  void $ many' (satisfy (notInClass lineEnding))
  void $ satisfy (inClass lineEnding)
  where
    lineEnding = "\r\n"


multiLineComment :: Parser ()
multiLineComment = do
  void $ string "{-"
  rest

  where
    rest :: Parser ()
    rest = void (string "-}") <|> (void anyChar >> rest)


token :: Parser Token
token =
  choice
  [ lambda
  , arrow
  , force
  , delay
  , openParen
  , closeParen
  , errorKeyword
  , byteStringLiteral
  , integerLiteral
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


force :: Parser Token
force = Force <$ char '!'


delay :: Parser Token
delay = Delay <$ char '#'


openParen :: Parser Token
openParen = OpenParen <$ char '('


closeParen :: Parser Token
closeParen = CloseParen <$ char ')'


errorKeyword :: Parser Token
errorKeyword = Error <$ string "Error"


integerLiteral :: Parser Token
integerLiteral = Integer <$> signed decimal


byteStringLiteral :: Parser Token
byteStringLiteral = hexadecimalByteStringLiteral


hexadecimalByteStringLiteral :: Parser Token
hexadecimalByteStringLiteral = do
  void $ string "0x"
  ByteString . BS.pack <$> many' hexByteLiteral


hexByteLiteral :: Parser Word8
hexByteLiteral = do
  a <- hexDigit
  b <- hexDigit
  return (a * 16 + b)


hexDigit :: Parser Word8
hexDigit = hexCharToWord8 <$> satisfy (inClass (['0'..'9'] <> ['a'..'f'] <> ['A'..'F']))


hexCharToWord8 :: Char -> Word8
hexCharToWord8 =
  \case
    '0' -> 0
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9
    'a' -> 10
    'A' -> 10
    'b' -> 11
    'B' -> 11
    'c' -> 12
    'C' -> 12
    'd' -> 13
    'D' -> 13
    'e' -> 14
    'E' -> 14
    'f' -> 15
    'F' -> 15
    _   -> 0


textLiteral :: Parser Token
textLiteral = do
  void $ char '"'
  chars <- many' character
  void $ char '"'
  return (Text (pack chars))

  where
    character :: Parser Char
    character = satisfy (notInClass specialChar) <|> escapeCode

    specialChar :: String
    specialChar = "\"\\\r\n\t"

    escapeCode :: Parser Char
    escapeCode = do
      void $ char '\\'
      literalEscapeCode <|> letterEscapeCode

    literalEscapeCode :: Parser Char
    literalEscapeCode = satisfy (inClass "\"\\")

    letterEscapeCode :: Parser Char
    letterEscapeCode = do
      c <- satisfy (inClass "rnt")
      return $
        case c of
          'r' -> '\r'
          'n' -> '\n'
          't' -> '\t'
          _   -> '\0'


boolLiteral :: Parser Token
boolLiteral = (Bool True <$ string "True") <|> (Bool False <$ string "False")


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


-- Maps a token to its unique syntactic form.
printToken :: Token -> Text
printToken =
  \case
    Var x          -> x
    Lambda         -> "\\"
    Arrow          -> "->"
    Force          -> "!"
    Delay          -> "#"
    OpenParen      -> "("
    CloseParen     -> ")"
    Error          -> "Error"
    Integer i      -> pack (show i)
    ByteString b   -> "0x" <> encodeHex b
    Text t         -> "\"" <> escapeText t <> "\""
    Bool True      -> "True"
    Bool False     -> "False"
    OpenBracket    -> "["
    CloseBracket   -> "]"
    Comma          -> ","
    OpenBrace      -> "{"
    CloseBrace     -> "}"
    Data           -> "data"
    Sigma          -> "sigma"
    Equals         -> "="
    Builtin b      -> printBuiltin b
    InfixBuiltin b -> printInfixBuiltin b
    Let            -> "let"
    Semicolon      -> ";"
    In             -> "in"
    If             -> "if"
    Then           -> "then"
    Else           -> "else"


escapeText :: Text -> Text
escapeText =
    replace "\"" "\\\""
  . replace "\\" "\\\\"
  . replace "\r" "\\r"
  . replace "\n" "\\n"
  . replace "\t" "\\t"


printBuiltin :: Builtin -> Text
printBuiltin = pack . show


printInfixBuiltin :: Infix.InfixBuiltin -> Text
printInfixBuiltin =
  \case
    Infix.AddInteger              -> "+i"
    Infix.SubtractInteger         -> "-i"
    Infix.MultiplyInteger         -> "*i"
    Infix.DivideInteger           -> "/i"
    Infix.RemainderInteger        -> "%i"
    Infix.EqualsInteger           -> "==i"
    Infix.LessThanInteger         -> "<i"
    Infix.LessThanEqualsInteger   -> "<=i"
    Infix.AppendByteString        -> "+b"
    Infix.ConsByteString          -> ":b"
    Infix.IndexByteString         -> "!b"
    Infix.EqualsByteString        -> "==b"
    Infix.LessThanByteString      -> "<b"
    Infix.LessThanEqualByteString -> "<=b"
    Infix.AppendString            -> "+s"
    Infix.EqualsString            -> "==s"
    Infix.EqualsData              -> "==d"
