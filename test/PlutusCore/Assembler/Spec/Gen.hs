{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}


module PlutusCore.Assembler.Spec.Gen
  ( genText
  , genToken
  , genName
  , genWhitespace
  , genData
  , genConstant
  , genTerm
  ) where


import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified PlutusCore.Data as Data

import PlutusCore.Assembler.Spec.Prelude
import PlutusCore.Assembler.Types.Token (Token (..))
import PlutusCore.Assembler.Types.AST (Data, Constant, Term, OpTerm, Binding)
import qualified PlutusCore.Assembler.Types.Constant as AST
import qualified PlutusCore.Assembler.Types.AST as AST


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


genConstant :: Gen Constant
genConstant =
  Gen.choice
  [ AST.I <$> genInteger
  , AST.S <$> genByteString
  , AST.T <$> genText
  , pure AST.U
  , pure (AST.B True)
  , pure (AST.B False)
  , AST.L <$> Gen.list (Range.linear 0 10) genConstant
  , AST.P <$> ((,) <$> genConstant <*> genConstant)
  , AST.D <$> genData
  ]


genBinding :: Gen Binding
genBinding = AST.Binding <$> (AST.Name <$> genName) <*> genTerm


genOpTerm :: Gen OpTerm
genOpTerm =
  AST.OpTerm
  <$> (      (AST.Builtin        <$> Gen.enumBounded)
         <|> (AST.Var . AST.Name <$> genName)           )


genTerm :: Gen Term
genTerm =
  Gen.choice
  [ AST.Var . AST.Name <$> genName
  , AST.Lambda <$> genBinding
  , AST.Apply <$> genTerm <*> genTerm
  , AST.Force <$> genTerm
  , AST.Delay <$> genTerm
  , AST.Constant <$> genConstant
  , AST.Builtin <$> Gen.enumBounded
  , pure AST.Error
  , AST.Let <$> (Gen.list (Range.linear 0 10) genBinding) <*> genTerm
  , AST.IfThenElse <$> (AST.IfTerm   <$> genTerm)
                   <*> (AST.ThenTerm <$> genTerm)
                   <*> (AST.ElseTerm <$> genTerm)
  , AST.InfixApply <$> (AST.LeftTerm <$> genTerm)
                   <*> genOpTerm
                   <*> (AST.RightTerm <$> genTerm)
  ]
