{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Parse ( parse ) where


import           Data.Either.Extra                       (mapLeft)
import           Data.Text                               (pack, unpack)
import qualified PlutusCore.Data                         as Data
import           Text.Parsec                             (Parsec, SourcePos,
                                                          many, option, try)
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


parse :: [(Token, SourcePos)] -> Either ErrorMessage Program
parse = mapLeft (ErrorMessage . pack . show) . Prim.parse program "input"


consume :: ((Token, SourcePos) -> Maybe a) -> Parser a
consume = token (unpack . printToken . fst) snd


consumeExact :: Token -> a -> Parser a
consumeExact tok tm =
  consume (\(t, _) -> guard (t == tok) >> return tm)


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


program :: Parser Program
program = AST.Program <$> term


term :: Parser Term
term = term0


term0 :: Parser Term
term0 = lambdaTerm <|> term1


lambdaTerm :: Parser Term
lambdaTerm = do
  consumeExact Tok.Lambda ()
  x <- consumeVar
  consumeExact Tok.Arrow ()
  AST.Lambda . AST.Binding (AST.Name x) <$> term


term1 :: Parser Term
term1 = ifTerm <|> letTerm <|> term2


ifTerm :: Parser Term
ifTerm = do
  consumeExact Tok.If ()
  i <- AST.IfTerm <$> term2
  consumeExact Tok.Then ()
  t <- AST.ThenTerm <$> term2
  consumeExact Tok.Else ()
  e <- AST.ElseTerm <$> term1
  return (AST.IfThenElse i t e)


letTerm :: Parser Term
letTerm = do
  consumeExact Tok.Let ()
  b0 <- letBinding
  bs <- many (consumeExact Tok.Semicolon () >> letBinding)
  consumeExact Tok.In ()
  AST.Let (b0:bs) <$> term2


letBinding :: Parser Binding
letBinding = do
  x <- AST.Name <$> consumeVar
  consumeExact Tok.Equals ()
  AST.Binding x <$> term2


term2 :: Parser Term
term2 = try infixApplyTerm <|> term3


infixApplyTerm :: Parser Term
infixApplyTerm = do
  t0 <- AST.LeftTerm  <$> term3
  op <- AST.OpTerm    <$> infixOperator
  t1 <- AST.RightTerm <$> term3
  return (AST.InfixApply t0 op t1)


infixOperator :: Parser Term
infixOperator = infixBuiltin <|> backtickInfix


infixBuiltin :: Parser Term
infixBuiltin =
  consume $
    \case
      (Tok.InfixBuiltin b, _) -> pure (AST.Builtin (InfixBuiltin.toBuiltin b))
      _                       -> mzero


backtickInfix :: Parser Term
backtickInfix = do
  consumeExact Tok.Backtick ()
  t <- varTerm <|> builtinTerm
  consumeExact Tok.Backtick ()
  return t


term3 :: Parser Term
term3 = do
  t0  <- term4
  ts <- many term4
  return $ foldl AST.Apply t0 ts


term4 :: Parser Term
term4 = forceTerm <|> delayTerm <|> term5


forceTerm :: Parser Term
forceTerm = consumeExact Tok.Force () >> (AST.Force <$> term4)


delayTerm :: Parser Term
delayTerm = consumeExact Tok.Delay () >> (AST.Delay <$> term4)


term5 :: Parser Term
term5 = varTerm <|> builtinTerm <|> errorTerm <|> try parenthesizedTerm <|> constantTerm


varTerm :: Parser Term
varTerm = AST.Var . AST.Name <$> consumeVar


constantTerm :: Parser Term
constantTerm = AST.Constant <$> constant


constant :: Parser Constant
constant = bool
       <|> integer
       <|> byteString
       <|> try unit
       <|> text
       <|> (AST.L <$> bracketList constant)
       <|> try tuple
       <|> try dataConstant


bool :: Parser Constant
bool = consumeExact (Tok.Bool True ) (AST.B True )
       <|> consumeExact (Tok.Bool False) (AST.B False)


integer :: Parser Constant
integer = AST.I <$> consumeInteger


byteString :: Parser Constant
byteString = AST.S <$> consumeByteString


unit :: Parser Constant
unit = do
  consumeExact Tok.OpenParen ()
  consumeExact Tok.CloseParen AST.U


text :: Parser Constant
text =
  consume $
    \case
      (Tok.Text t, _) -> pure (AST.T t)
      _               -> mzero


bracketList :: Parser a -> Parser [a]
bracketList p = do
  consumeExact Tok.OpenBracket ()
  l <- list p
  consumeExact Tok.CloseBracket ()
  return l


list :: Parser a -> Parser [a]
list p = do
  option [] $ do
    l0 <- p
    ls <-
      many $ do
        consumeExact Tok.Comma ()
        p
    return (l0:ls)


tuple :: Parser Constant
tuple = do
  consumeExact Tok.OpenParen ()
  p0 <- constant
  consumeExact Tok.Comma ()
  p1 <- constant
  consumeExact Tok.CloseParen ()
  return (AST.P (p0, p1))


dataConstant :: Parser Constant
dataConstant = do
  consumeExact Tok.Data ()
  AST.D <$> dataLiteral


dataLiteral :: Parser Data
dataLiteral = sigmaData <|> listData <|> mapData <|> integerData <|> byteStringData


sigmaData :: Parser Data
sigmaData = do
  consumeExact Tok.Sigma ()
  i <- consumeInteger
  consumeExact Tok.Period ()
  Data.Constr i <$> bracketList dataLiteral


listData :: Parser Data
listData = Data.List <$> bracketList dataLiteral


mapData :: Parser Data
mapData = do
  consumeExact Tok.OpenBrace ()
  es <- list mapEntry
  consumeExact Tok.CloseBrace ()
  return (Data.Map es)


mapEntry :: Parser (Data, Data)
mapEntry = do
  k <- dataLiteral
  consumeExact Tok.Equals ()
  v <- dataLiteral
  return (k,v)


integerData :: Parser Data
integerData = Data.I <$> consumeInteger


byteStringData :: Parser Data
byteStringData = Data.B <$> consumeByteString


builtinTerm :: Parser Term
builtinTerm =
  consume $
    \case
      (Tok.Builtin b, _) -> pure (AST.Builtin b)
      _                  -> mzero


errorTerm :: Parser Term
errorTerm = consumeExact Tok.Error AST.Error


parenthesizedTerm :: Parser Term
parenthesizedTerm = do
  consumeExact Tok.OpenParen ()
  t <- term
  consumeExact Tok.CloseParen ()
  return t
