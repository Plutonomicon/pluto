{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}


module PlutusCore.Assembler.Spec.TokenizeSpec ( tests ) where


import qualified Hedgehog.Gen                      as Gen
import qualified Hedgehog.Range                    as Range

import           PlutusCore.Assembler.Spec.Prelude
import           PlutusCore.Assembler.Tokenize     (printToken, tokenize)
import           PlutusCore.Assembler.Types.Token  (Token (..))


genText :: Gen Text
genText = Gen.text (Range.linear 0 1000) Gen.ascii


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
  , Integer <$> Gen.integral (Range.linear (-100_000_000_000) 100_000_000_000)
  , ByteString <$> Gen.bytes (Range.linear 0 1000)
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


tests :: TestTree
tests =
  testGroup "tokenize"
  [ commutesWithConcatByWhitespaceTest
  , prependWhitespaceTest
  , appendWhitespaceTest
  , tokenTest
  ]


commutesWithConcatByWhitespaceTest :: TestTree
commutesWithConcatByWhitespaceTest =
  testProperty "commutes with concatenating strings separated by whitespace in case of a successful parse" . property $ do
    t0 <- forAll genText
    t1 <- forAll genText
    w  <- forAll genWhitespace
    case (liftA2 (<>) (f t0) (f t1), f (t0 <> w <> t1)) of
      (Just x, Just y) -> x === y
      _ -> return ()
  where f = fmap (fmap fst) . eitherToMaybe . tokenize


prependWhitespaceTest :: TestTree
prependWhitespaceTest =
  testProperty "result is unaffected by prepending whitespace" . property $ do
    t <- forAll genText
    w <- forAll genWhitespace
    (fst <$$> eitherToMaybe (tokenize t)) === (fst <$$> eitherToMaybe (tokenize (w <> t)))


appendWhitespaceTest :: TestTree
appendWhitespaceTest =
  testProperty "result is unaffected by appending whitespace" . property $ do
    t <- forAll genText
    w <- forAll genWhitespace
    (fst <$$> eitherToMaybe (tokenize t)) === (fst <$$> eitherToMaybe (tokenize (w <> t)))


tokenTest :: TestTree
tokenTest =
  testProperty "tokenizes a single token" . property $ do
    t <- forAll genToken
    (fst <$$> tokenize (printToken t)) === Right [t]
