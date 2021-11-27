{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module PlutusCore.Assembler.Spec.TokenizeSpec ( tests ) where


import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Spec.Gen     (genText, genToken,
                                                    genWhitespace)
import           PlutusCore.Assembler.Spec.Prelude
import           PlutusCore.Assembler.Tokenize     (printToken, tokenize)


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
      _                -> return ()
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
