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


import qualified Data.ByteString                         as BS
import           Data.Either.Combinators                 (mapLeft)
import           Data.Text                               (cons, pack, replace)
import           Data.Word                               (Word8)
import           Text.Hex                                (encodeHex)
import           Text.Parsec                             (SourcePos, choice,
                                                          eof, many1)
import           Text.Parsec.Char                        (anyChar, char, noneOf,
                                                          oneOf, string)
import           Text.Parsec.Prim                        (getPosition,
                                                          lookAhead, many,
                                                          parse, try)
import           Text.Parsec.Text                        (Parser)

import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Types.Builtin      (Builtin (..))
import           PlutusCore.Assembler.Types.ErrorMessage (ErrorMessage (..))
import qualified PlutusCore.Assembler.Types.InfixBuiltin as Infix
import           PlutusCore.Assembler.Types.Token        (Token (..))


-- TODO: convert to Parsec and output [(Token, SourcePos)]


tokenize :: Text -> Either ErrorMessage [(Token, SourcePos)]
tokenize = mapLeft (ErrorMessage . pack . show) . parse tokens "input"


tokens :: Parser [(Token, SourcePos)]
tokens = do
  void (many whitespace)
  ts <- many $ do
         t <- token
         p <- getPosition
         void (many whitespace)
         return (t, p)
  eof
  return ts


whitespace :: Parser ()
whitespace = void (oneOf " \t\r\n") <|> try oneLineComment <|> try multiLineComment


oneLineComment :: Parser ()
oneLineComment = do
  void $ string "--"
  void $ many (noneOf lineEnding)
  void $ oneOf lineEnding
  where
    lineEnding = "\r\n"


multiLineComment :: Parser ()
multiLineComment = do
  void $ string "{-"
  rest

  where
    rest :: Parser ()
    rest = void (try (string "-}")) <|> (void anyChar >> rest)


token :: Parser Token
token =
  choice
  $
  try
  <$>
  [ lambda
  , arrow
  -- infixBuiltin must come before force to deal with ambiguity
  , infixBuiltin
  , force
  , delay
  , openParen
  , closeParen
  , errorKeyword
  -- byteStringLiteral must come before integerLiteral to deal with ambiguity
  , byteStringLiteral
  , integerLiteral
  , textLiteral
  , boolLiteral
  , openBracket
  , closeBracket
  , comma
  , period
  , backtick
  , openBrace
  , closeBrace
  , dataKeyword
  , sigmaKeyword
  , equals
  , builtin
  , letKeyword
  , semicolon
  , inKeyword
  , ifKeyword
  , thenKeyword
  , elseKeyword
  -- var must come last in order to deal with ambiguity
  , var
  ]


var :: Parser Token
var = do
  begin <- oneOf ['a'..'z']
  rest  <- many (oneOf (['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "_"))
  return (Var (cons begin (pack rest)))


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
integerLiteral = Integer <$> (positiveIntegerLiteral <|> negativeIntegerLiteral)


negativeIntegerLiteral :: Parser Integer
negativeIntegerLiteral = do
  void (char '-')
  negate <$> positiveIntegerLiteral


positiveIntegerLiteral :: Parser Integer
positiveIntegerLiteral =
  digitsToInteger <$> many1 (oneOf ['0'..'9'])


digitToInteger :: Char -> Integer
digitToInteger =
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
    _   -> 0


digitsToInteger :: String -> Integer
digitsToInteger = foldl (\a x -> a * 10 + x) 0 . fmap digitToInteger


byteStringLiteral :: Parser Token
byteStringLiteral = hexadecimalByteStringLiteral


hexadecimalByteStringLiteral :: Parser Token
hexadecimalByteStringLiteral = do
  void $ string "0x"
  ByteString . BS.pack <$> many hexByteLiteral


hexByteLiteral :: Parser Word8
hexByteLiteral = do
  a <- hexDigit
  b <- hexDigit
  return (a * 16 + b)


hexDigit :: Parser Word8
hexDigit = hexCharToWord8 <$> oneOf (['0'..'9'] <> ['a'..'f'] <> ['A'..'F'])


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
  chars <- many character
  void $ char '"'
  return (Text (pack chars))

  where
    character :: Parser Char
    character = noneOf specialChar <|> escapeCode

    specialChar :: String
    specialChar = "\"\\\r\n\t"

    escapeCode :: Parser Char
    escapeCode = do
      void $ char '\\'
      literalEscapeCode <|> letterEscapeCode

    literalEscapeCode :: Parser Char
    literalEscapeCode = oneOf "\"\\"

    letterEscapeCode :: Parser Char
    letterEscapeCode = do
      c <- oneOf "rnt"
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


period :: Parser Token
period = Period <$ char '.'


backtick :: Parser Token
backtick = Backtick <$ char '`'


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
  Builtin
  <$>
  (
  choice
  $
  try
  <$>
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
  , UnBData <$ string "UnBData"
  , EqualsData <$ string "EqualsData"
  , MkPairData <$ string "MkPairData"
  , MkNilData <$ string "MkNilData"
  , MkNilPairData <$ string "MkNilPairData"
  ]
  )


infixBuiltin :: Parser Token
infixBuiltin =
  InfixBuiltin
  <$>
  (
  choice
  $
  try
  <$>
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
  )


letKeyword :: Parser Token
letKeyword = Let <$ peekNonName (string "let")


semicolon :: Parser Token
semicolon = Semicolon <$ char ';'


inKeyword :: Parser Token
inKeyword = In <$ peekNonName (string "in")


ifKeyword :: Parser Token
ifKeyword = If <$ peekNonName (string "if")


thenKeyword :: Parser Token
thenKeyword = Then <$ peekNonName (string "then")


elseKeyword :: Parser Token
elseKeyword = Else <$ peekNonName (string "else")


-- This parser consumes no input but resolves parsing ambiguity by not accepting the parse
-- if the thing we are parsing could be a variable name.
peekNonName :: Parser a -> Parser a
peekNonName p = do
  x  <- p
  mc <- (Just <$> lookAhead anyChar) <|> (eof >> pure Nothing)
  case mc of
    Nothing -> return x
    Just c ->
      if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9') || c == '_'
      then mzero
      else return x


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
    Period         -> "."
    Backtick       -> "`"
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
  . replace "\r" "\\r"
  . replace "\n" "\\n"
  . replace "\t" "\\t"
  . replace "\\" "\\\\"


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
