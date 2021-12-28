{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Parse ( parse, parseData ) where


import           Data.Either.Extra                       (mapLeft)
import           Data.Text                               (pack, unpack)
import qualified PlutusCore.Data                         as Data
import           Text.Parsec                             (Parsec, SourceName,
                                                          SourcePos, eof,
                                                          getPosition, many,
                                                          many1, option, try)
import           Text.Parsec.Prim                        (token)
import qualified Text.Parsec.Prim                        as Prim

import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Tokenize           (printToken)
import           PlutusCore.Assembler.Types.AST          (Binding, Constant,
                                                          Data, Program, Term)
import qualified PlutusCore.Assembler.Types.AST          as AST
import           PlutusCore.Assembler.Types.ErrorMessage (ErrorMessage (..))
import qualified PlutusCore.Assembler.Types.InfixBuiltin as InfixBuiltin
import           PlutusCore.Assembler.Types.Token        (Token)
import qualified PlutusCore.Assembler.Types.Token        as Tok


type Parser = Parsec [(Token, SourcePos)] ()


parse :: SourceName -> [(Token, SourcePos)] -> Either ErrorMessage (Program SourcePos)
parse = parse' program

parseData :: SourceName -> [(Token, SourcePos)] -> Either ErrorMessage Data
parseData = parse' dataLiteral

parse' :: Parser a -> SourceName -> [(Token, SourcePos)] -> Either ErrorMessage a
parse' p name = mapLeft (ErrorMessage . pack . show) . Prim.parse p name

consume :: ((Token, SourcePos) -> Maybe a) -> Parser a
consume = token (unpack . printToken . fst) snd


consumeExact :: Token -> (SourcePos -> a) -> Parser a
consumeExact tok tm =
  consume (\(t, p) -> guard (t == tok) >> return (tm p))


consumeExact_ :: Token -> Parser ()
consumeExact_ tok = consumeExact tok (const ())


consumeInteger :: Parser Integer
consumeInteger =
  consume $
    \case
      (Tok.Integer i, _) -> pure i
      _                  -> mzero


consumeByteString :: Parser ByteString
consumeByteString =
  consume $
    \case
      (Tok.ByteString b, _) -> pure b
      _                     -> mzero


consumeVar :: Parser Text
consumeVar =
  consume $
    \case
      (Tok.Var x, _) -> pure x
      _              -> mzero


program :: Parser (Program SourcePos)
program = do
  p <- AST.Program <$> term
  eof
  return p


term :: Parser (Term SourcePos)
term = term0


term0 :: Parser (Term SourcePos)
term0 = lambdaTerm <|> term1


lambdaTerm :: Parser (Term SourcePos)
lambdaTerm = do
  p <- getPosition
  consumeExact_ Tok.Lambda
  xs <- many1 consumeVar
  consumeExact_ Tok.Arrow
  AST.Lambda p (AST.Name <$> xs) <$> term


term1 :: Parser (Term SourcePos)
term1 = ifTerm <|> letTerm <|> term2


ifTerm :: Parser (Term SourcePos)
ifTerm = do
  p <- getPosition
  consumeExact_ Tok.If
  i <- AST.IfTerm <$> term2
  consumeExact_ Tok.Then
  t <- AST.ThenTerm <$> term2
  consumeExact_ Tok.Else
  e <- AST.ElseTerm <$> term1
  return (AST.IfThenElse p i t e)


letTerm :: Parser (Term SourcePos)
letTerm = do
  p <- getPosition
  consumeExact_ Tok.Let
  b0 <- letBinding
  bs <- many (consumeExact_ Tok.Semicolon >> letBinding)
  consumeExact_ Tok.In
  AST.Let p (b0:bs) <$> term2


letBinding :: Parser (Binding SourcePos)
letBinding = do
  p <- getPosition
  x <- AST.Name <$> consumeVar
  consumeExact_ Tok.Equals
  AST.Binding p x <$> term2


term2 :: Parser (Term SourcePos)
term2 = try infixApplyTerm <|> term3


infixApplyTerm :: Parser (Term SourcePos)
infixApplyTerm = do
  p <- getPosition
  t0 <- AST.LeftTerm  <$> term3
  op <- AST.OpTerm    <$> infixOperator
  t1 <- AST.RightTerm <$> term3
  return (AST.InfixApply p t0 op t1)


infixOperator :: Parser (Term SourcePos)
infixOperator = infixBuiltin <|> backtickInfix


infixBuiltin :: Parser (Term SourcePos)
infixBuiltin =
  consume $
    \case
      (Tok.InfixBuiltin b, p) -> pure (AST.Builtin p (InfixBuiltin.toBuiltin b))
      _                       -> mzero


backtickInfix :: Parser (Term SourcePos)
backtickInfix = do
  consumeExact_ Tok.Backtick
  t <- varTerm <|> builtinTerm
  consumeExact_ Tok.Backtick
  return t


term3 :: Parser (Term SourcePos)
term3 = do
  p <- getPosition
  t0  <- term4
  ts <- many term4
  return $ foldl (AST.Apply p) t0 ts


term4 :: Parser (Term SourcePos)
term4 = forceTerm <|> delayTerm <|> term5


forceTerm :: Parser (Term SourcePos)
forceTerm = do
  p <- getPosition
  consumeExact_ Tok.Force >> (AST.Force p <$> term4)


delayTerm :: Parser (Term SourcePos)
delayTerm = do
  p <- getPosition
  consumeExact_ Tok.Delay >> (AST.Delay p <$> term4)


term5 :: Parser (Term SourcePos)
term5 = varTerm <|> builtinTerm <|> errorTerm <|> try parenthesizedTerm <|> constantTerm


varTerm :: Parser (Term SourcePos)
varTerm = do
  p <- getPosition
  AST.Var p . AST.Name <$> consumeVar


constantTerm :: Parser (Term SourcePos)
constantTerm = do
  p <- getPosition
  AST.Constant p <$> constant


constant :: Parser (Constant SourcePos)
constant =
      bool
  <|> integer
  <|> byteString
  <|> try unit
  <|> text
  <|> try dataConstant


bool :: Parser (Constant SourcePos)
bool = consumeExact (Tok.Bool True ) (`AST.B` True)
   <|> consumeExact (Tok.Bool False) (`AST.B` False)


integer :: Parser (Constant SourcePos)
integer = do
  p <- getPosition
  AST.I p <$> consumeInteger


byteString :: Parser (Constant SourcePos)
byteString = do
  p <- getPosition
  AST.S p <$> consumeByteString


unit :: Parser (Constant SourcePos)
unit = do
  p <- getPosition
  consumeExact_ Tok.OpenParen
  consumeExact_ Tok.CloseParen
  return (AST.U p)


text :: Parser (Constant SourcePos)
text =
  consume $
    \case
      (Tok.Text t, p) -> pure (AST.T p t)
      _               -> mzero


bracketList :: Parser a -> Parser [a]
bracketList p = do
  consumeExact_ Tok.OpenBracket
  l <- list p
  consumeExact_ Tok.CloseBracket
  return l


list :: Parser a -> Parser [a]
list p = do
  option [] $ do
    l0 <- p
    ls <-
      many $ do
        consumeExact_ Tok.Comma
        p
    return (l0:ls)


dataConstant :: Parser (Constant SourcePos)
dataConstant = do
  p <- getPosition
  consumeExact_ Tok.Data
  AST.D p <$> dataLiteral


dataLiteral :: Parser Data
dataLiteral = sigmaData <|> listData <|> mapData <|> integerData <|> byteStringData


sigmaData :: Parser Data
sigmaData = do
  consumeExact_ Tok.Sigma
  i <- consumeInteger
  consumeExact_ Tok.Period
  Data.Constr i <$> bracketList dataLiteral


listData :: Parser Data
listData = Data.List <$> bracketList dataLiteral


mapData :: Parser Data
mapData = do
  consumeExact_ Tok.OpenBrace
  es <- list mapEntry
  consumeExact_ Tok.CloseBrace
  return (Data.Map es)


mapEntry :: Parser (Data, Data)
mapEntry = do
  k <- dataLiteral
  consumeExact_ Tok.Equals
  v <- dataLiteral
  return (k,v)


integerData :: Parser Data
integerData = Data.I <$> consumeInteger


byteStringData :: Parser Data
byteStringData = Data.B <$> consumeByteString


builtinTerm :: Parser (Term SourcePos)
builtinTerm =
  consume $
    \case
      (Tok.Builtin b, p) -> pure (AST.Builtin p b)
      _                  -> mzero


errorTerm :: Parser (Term SourcePos)
errorTerm = consumeExact Tok.Error AST.Error


parenthesizedTerm :: Parser (Term SourcePos)
parenthesizedTerm = do
  consumeExact_ Tok.OpenParen
  t <- term
  consumeExact_ Tok.CloseParen
  return t
