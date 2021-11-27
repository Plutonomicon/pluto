{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}


module PlutusCore.Assembler.Spec.Gen
  ( genText
  , genToken
  , genName
  , genWhitespace
  , genData
  , genConstantAST
  , genTerm
  ) where


import           Data.List                               (intercalate)
import qualified Hedgehog.Gen                            as Gen
import qualified Hedgehog.Range                          as Range
import qualified PlutusCore.Data                         as Data

import           PlutusCore.Assembler.ConstantToTokens   (constantToTokens)
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Spec.Prelude
import           PlutusCore.Assembler.Types.AST          (Binding, Constant,
                                                          Data, Term)
import qualified PlutusCore.Assembler.Types.AST          as AST
import qualified PlutusCore.Assembler.Types.InfixBuiltin as InfixBuiltin
import           PlutusCore.Assembler.Types.Token        (Token (..))
import qualified PlutusCore.Assembler.Types.Token        as Tok


-- Passed to a generator, indicates the maximum recursion depth its children should have.
newtype RecursionDepth = RecursionDepth { unRecursionDepth :: Integer }
  deriving (Eq, Show, Ord, Enum, Num, Real, Integral)


genText :: Gen Text
genText = Gen.text (Range.linear 0 100) Gen.ascii


genInteger :: Gen Integer
genInteger = Gen.integral (Range.linear (-100_000_000_000) 100_000_000_000)


genByteString :: Gen ByteString
genByteString = Gen.bytes (Range.linear 0 100)


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


genData :: RecursionDepth -> Gen Data
genData 0 =
  Gen.choice
  [ Data.I <$> genInteger
  , Data.B <$> genByteString
  ]
genData n =
  Gen.choice
  [ Data.I <$> genInteger
  , Data.B <$> genByteString
  , Data.List <$> Gen.list (Range.linear 0 10) (genData (n-1))
  , Data.Map <$> Gen.list (Range.linear 0 10)
                 ((,) <$> genData (n-1) <*> genData (n-1))
  , Data.Constr <$> genInteger <*> Gen.list (Range.linear 0 10) (genData (n-1))
  ]


genConstantAST :: RecursionDepth -> Gen (Constant ())
genConstantAST 0 =
  Gen.choice
  [ AST.I () <$> genInteger
  , AST.S () <$> genByteString
  , AST.T () <$> genText
  , pure (AST.U ())
  , pure (AST.B () True)
  , pure (AST.B () False)
  ]
genConstantAST n =
  Gen.choice
  [ AST.I () <$> genInteger
  , AST.S () <$> genByteString
  , AST.T () <$> genText
  , pure (AST.U ())
  , pure (AST.B () True)
  , pure (AST.B () False)
  , AST.L () <$> Gen.list (Range.linear 0 10) (genConstantAST (n-1))
  , AST.P () <$> ((,) <$> genConstantAST (n-1) <*> genConstantAST (n-1))
  , AST.D () <$> genData (n-1)
  ]


-- Generates a syntactically valid token string and the term
-- it represents.
genTerm :: RecursionDepth -> Gen (Term (), [Token])
genTerm = genTerm0


genTerm0 :: RecursionDepth -> Gen (Term (), [Token])
genTerm0 0 = genTerm1 0
genTerm0 n = Gen.choice [genLambda (n-1), genTerm1 n]


genLambda :: RecursionDepth -> Gen (Term (), [Token])
genLambda n = do
  xs <- Gen.list (Range.linear 1 10) genName
  (y, yts) <- genTerm n
  return
    ( AST.Lambda () (AST.Name <$> xs) y
    , [Tok.Lambda] <> (Tok.Var <$> xs) <> [Tok.Arrow] <> yts
    )


genTerm1 :: RecursionDepth -> Gen (Term (), [Token])
genTerm1 0 = genTerm2 0
genTerm1 n = Gen.choice [genIfTerm (n-1), genLetTerm (n-1), genTerm2 n]


genIfTerm :: RecursionDepth -> Gen (Term (), [Token])
genIfTerm n = do
  (it, itts) <- first AST.IfTerm <$> genTerm2 n
  (tt, ttts) <- first AST.ThenTerm <$> genTerm2 n
  (et, etts) <- first AST.ElseTerm <$> genTerm1 n
  return
    ( AST.IfThenElse () it tt et
    , [Tok.If] <> itts <> [Tok.Then] <> ttts <> [Tok.Else] <> etts
    )


genLetTerm :: RecursionDepth -> Gen (Term (), [Token])
genLetTerm n = do
  (bs, bsts) <- unzip <$> Gen.list (Range.linear 1 10) (genLetBinding n)
  (t, tts)   <- genTerm2 n
  return
    ( AST.Let () bs t
    , [Tok.Let] <> intercalate [Tok.Semicolon] bsts <> [Tok.In] <> tts
    )


genLetBinding :: RecursionDepth -> Gen (Binding (), [Token])
genLetBinding n = do
  x        <- genName
  (t, tts) <- genTerm2 n
  return
    ( AST.Binding () (AST.Name x) t
    , [Tok.Var x, Tok.Equals] <> tts
    )


genTerm2 :: RecursionDepth -> Gen (Term (), [Token])
genTerm2 0 = genTerm3 0
genTerm2 n = Gen.choice [genInfixApply (n-1), genTerm3 n]


genInfixApply :: RecursionDepth -> Gen (Term (), [Token])
genInfixApply n =
  Gen.choice
  [ genBuiltinInfixOpApply n
  , genBuiltinBacktickInfixApply n
  , genVarInfixApply n
  ]


genBuiltinInfixOpApply :: RecursionDepth -> Gen (Term (), [Token])
genBuiltinInfixOpApply n = do
  (x, tsx) <- genTerm3 n
  (y, tsy) <- genTerm3 n
  o <- Gen.enumBounded
  return ( AST.InfixApply ()
                          (AST.LeftTerm x)
                          (AST.OpTerm (AST.Builtin () (InfixBuiltin.toBuiltin o)))
                          (AST.RightTerm y)
         , tsx <> [Tok.InfixBuiltin o] <> tsy
         )


genBuiltinBacktickInfixApply :: RecursionDepth -> Gen (Term (), [Token])
genBuiltinBacktickInfixApply n = do
  (x, tsx) <- genTerm3 n
  (y, tsy) <- genTerm3 n
  o <- Gen.enumBounded
  return ( AST.InfixApply ()
                          (AST.LeftTerm x)
                          (AST.OpTerm (AST.Builtin () o))
                          (AST.RightTerm y)
         , tsx <> [Tok.Backtick, Tok.Builtin o, Tok.Backtick] <> tsy
         )


genVarInfixApply :: RecursionDepth -> Gen (Term (), [Token])
genVarInfixApply n = do
  (x, tsx) <- genTerm3 n
  (y, tsy) <- genTerm3 n
  o <- genName
  return ( AST.InfixApply ()
                          (AST.LeftTerm x)
                          (AST.OpTerm (AST.Var () (AST.Name o)))
                          (AST.RightTerm y)
         , tsx <> [Tok.Backtick, Tok.Var o, Tok.Backtick] <> tsy
         )


genTerm3 :: RecursionDepth -> Gen (Term (), [Token])
genTerm3 n = do
  (x0, ts0) <- genTerm4 n
  ts        <- Gen.list (Range.linear 0 10) (genTerm4 n)
  return $ (foldl (AST.Apply ()) x0 *** foldl (<>) ts0) (unzip ts)


genTerm4 :: RecursionDepth -> Gen (Term (), [Token])
genTerm4 0 = genTerm5 0
genTerm4 n =
  Gen.choice
  [ genForce (n-1)
  , genDelay (n-1)
  , genTerm5 n
  ]


genForce :: RecursionDepth -> Gen (Term (), [Token])
genForce n = do
  (x, ts) <- genTerm4 n
  return (AST.Force () x, [Tok.Force] <> ts)


genDelay :: RecursionDepth -> Gen (Term (), [Token])
genDelay n = do
  (x, ts) <- genTerm4 n
  return (AST.Delay () x, [Tok.Delay] <> ts)


genTerm5 :: RecursionDepth -> Gen (Term (), [Token])
genTerm5 0 =
  Gen.choice
  [ genVarTerm
  , genBuiltinTerm
  , genErrorTerm
  , genConstantTerm 0
  ]
genTerm5 n =
  Gen.choice
  [ genVarTerm
  , genBuiltinTerm
  , genErrorTerm
  , genParenthesizedTerm (n-1)
  , genConstantTerm n
  ]


genVarTerm :: Gen (Term (), [Token])
genVarTerm = do
  x <- genName
  return (AST.Var () (AST.Name x), [Tok.Var x])


genBuiltinTerm :: Gen (Term (), [Token])
genBuiltinTerm = do
  b <- Gen.enumBounded
  return (AST.Builtin () b, [Tok.Builtin b])


genErrorTerm :: Gen (Term (), [Token])
genErrorTerm = pure (AST.Error (), [Tok.Error])


genParenthesizedTerm :: RecursionDepth -> Gen (Term (), [Token])
genParenthesizedTerm n = do
  (x, ts) <- genTerm n
  return (x, [Tok.OpenParen] <> ts <> [Tok.CloseParen])


genConstantTerm :: RecursionDepth -> Gen (Term (), [Token])
genConstantTerm n = do
  t <- genConstantAST n
  return (AST.Constant () t, constantToTokens t)
