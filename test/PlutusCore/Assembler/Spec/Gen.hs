{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}


module PlutusCore.Assembler.Spec.Gen
  ( genText
  , genToken
  , genName
  , genWhitespace
  , genData
  , genConstantAST
  , genTermAST
  , genTerm
  ) where


import Control.Arrow ((***))
import Data.List (unzip)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified PlutusCore.Data as Data

import PlutusCore.Assembler.Spec.Prelude
import PlutusCore.Assembler.Types.Token (Token (..))
import PlutusCore.Assembler.Types.AST (Data, Constant, Term, OpTerm, Binding)
import qualified PlutusCore.Assembler.Types.Constant as AST
import qualified PlutusCore.Assembler.Types.AST as AST
import qualified PlutusCore.Assembler.Types.Token as Tok


genText :: Gen Text
genText = Gen.text (Range.linear 0 1000) Gen.ascii


genInteger :: Gen Integer
genInteger = Gen.integral (Range.linear (-100_000_000_000) 100_000_000_000)


genByteString :: Gen ByteString
genByteString = Gen.bytes (Range.linear 0 1000)


genToken :: Gen Token
genToken =
  Gen.choice
  [ Var <$> genName
  , pure Lambda
  , pure Arrow
  , pure Force
  , pure Delay
  , pure OpenParen
  , pure CloseParen
  , pure Error
  , Integer <$> genInteger
  , ByteString <$> genByteString
  , Text <$> genText
  , pure (Bool True)
  , pure (Bool False)
  , pure OpenBracket
  , pure CloseBracket
  , pure Comma
  , pure Period
  , pure OpenBrace
  , pure CloseBrace
  , pure Data
  , pure Sigma
  , pure Equals
  , Builtin <$> Gen.enumBounded
  , InfixBuiltin <$> Gen.enumBounded
  , pure Let
  , pure Semicolon
  , pure In
  , pure If
  , pure Then
  , pure Else
  ]


genName :: Gen Text
genName = do
  nm <- (<>) <$> Gen.text (Range.singleton 1) (Gen.element ['a'..'z'])
              <*> Gen.text (Range.linear 0 100)
                    (Gen.element (['a'..'z']<>['A'..'Z']<>['0'..'9']<>['_']))
  return $ case nm of
    "if"    -> "iff"
    "then"  -> "thenn"
    "else"  -> "elsee"
    "in"    -> "inn"
    "data"  -> "dataa"
    "sigma" -> "sigmaa"
    x       -> x


genWhitespace :: Gen Text
genWhitespace = Gen.choice
  [ Gen.text (Range.singleton 1) (Gen.element [' ', '\r', '\n'])
  , genSingleLineComment
  , genMultiLineComment
  ]


genSingleLineComment :: Gen Text
genSingleLineComment =
  (<> "\n") . ("--" <>) <$>
  Gen.text (Range.linear 0 1000)
    (Gen.element [ c | c <- ['\0'..'\xff'], c /= '\r' && c /= '\n' ])


genMultiLineComment :: Gen Text
genMultiLineComment =
  (<> "-}") . ("{-" <>) <$>
  Gen.text (Range.linear 0 1000)
    (Gen.element [ c | c <- ['\0'..'\xff'], c /= '}' ])


genData :: Gen Data
genData =
  Gen.choice
  [ Data.I <$> genInteger
  , Data.B <$> genByteString
  , Data.List <$> Gen.list (Range.linear 0 10) genData
  , Data.Map <$> Gen.list (Range.linear 0 10)
                 ((,) <$> genData <*> genData)
  , Data.Constr <$> genInteger <*> Gen.list (Range.linear 0 10) genData
  ]


genConstantAST :: Gen Constant
genConstantAST =
  Gen.choice
  [ AST.I <$> genInteger
  , AST.S <$> genByteString
  , AST.T <$> genText
  , pure AST.U
  , pure (AST.B True)
  , pure (AST.B False)
  , AST.L <$> Gen.list (Range.linear 0 10) genConstantAST
  , AST.P <$> ((,) <$> genConstantAST <*> genConstantAST)
  , AST.D <$> genData
  ]


genBindingAST :: Gen Binding
genBindingAST = AST.Binding <$> (AST.Name <$> genName) <*> genTermAST


genOpTermAST :: Gen OpTerm
genOpTermAST =
  AST.OpTerm
  <$> (      (AST.Builtin        <$> Gen.enumBounded)
         <|> (AST.Var . AST.Name <$> genName)           )


genTermAST :: Gen Term
genTermAST =
  Gen.choice
  [ AST.Var . AST.Name <$> genName
  , AST.Lambda <$> genBindingAST
  , AST.Apply <$> genTermAST <*> genTermAST
  , AST.Force <$> genTermAST
  , AST.Delay <$> genTermAST
  , AST.Constant <$> genConstantAST
  , AST.Builtin <$> Gen.enumBounded
  , pure AST.Error
  , AST.Let <$> (Gen.list (Range.linear 0 10) genBindingAST) <*> genTermAST
  , AST.IfThenElse <$> (AST.IfTerm   <$> genTermAST)
                   <*> (AST.ThenTerm <$> genTermAST)
                   <*> (AST.ElseTerm <$> genTermAST)
  , AST.InfixApply <$> (AST.LeftTerm <$> genTermAST)
                   <*> genOpTermAST
                   <*> (AST.RightTerm <$> genTermAST)
  ]


-- Generates a syntactically valid token string and the term
-- it represents.
genTerm :: Gen (Term, [Token])
genTerm = genTerm0


genTerm0 :: Gen (Term, [Token])
genTerm0 = genLambda <|> genTerm1


genLambda :: Gen (Term, [Token])
genLambda = do
  x <- genName
  (y, ts) <- genTerm
  return
    ( AST.Lambda (AST.Binding (AST.Name x) y)
    , [ Tok.Lambda, Tok.Var x, Tok.Arrow ] <> ts
    )


genTerm1 :: Gen (Term, [Token])
genTerm1 = do
  (x0, ts0) <- genTerm2
  ts        <- Gen.list (Range.linear 0 10) genTerm2
  return $ (foldl AST.Apply x0 *** foldl (<>) ts0) (unzip ts)


genTerm2 :: Gen (Term, [Token])
genTerm2 = genIfTerm <|> genLetTerm <|> genTerm3


genIfTerm :: Gen (Term, [Token])
genIfTerm = todo


genLetTerm :: Gen (Term, [Token])
genLetTerm = todo


genTerm3 :: Gen (Term, [Token])
genTerm3 = genInfixApply <|> genTerm4


genInfixApply :: Gen (Term, [Token])
genInfixApply = todo


genTerm4 :: Gen (Term, [Token])
genTerm4 = genForce <|> genDelay <|> genTerm5


genForce :: Gen (Term, [Token])
genForce = todo


genDelay :: Gen (Term, [Token])
genDelay = todo


genTerm5 :: Gen (Term, [Token])
genTerm5 = genVarTerm <|> genBuiltinTerm <|> genErrorTerm <|> genParenthesizedTerm <|> genConstantTerm


genVarTerm :: Gen (Term, [Token])
genVarTerm = todo


genBuiltinTerm :: Gen (Term, [Token])
genBuiltinTerm = todo


genErrorTerm :: Gen (Term, [Token])
genErrorTerm = todo


genParenthesizedTerm :: Gen (Term, [Token])
genParenthesizedTerm = todo


genConstantTerm :: Gen (Term, [Token])
genConstantTerm = todo


todo :: a
todo = todo
