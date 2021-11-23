{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module PlutusCore.Assembler.Spec.TokenizeSpec ( tests ) where


import           Control.Applicative               (liftA2)
import           Data.Either.Extra                 (eitherToMaybe)
import qualified Hedgehog.Gen                      as Gen
import qualified Hedgehog.Range                    as Range

import           PlutusCore.Assembler.Spec.Prelude
import           PlutusCore.Assembler.Tokenize     (tokenize)


genText :: Gen Text
genText = Gen.text (Range.linear 0 1000) Gen.ascii


genWhitespace :: Gen Text
genWhitespace = Gen.choice
  [ Gen.text (Range.singleton 1) (Gen.element [' ', '\r', '\n'])
  , genSingleLineComment
  , genMultiLineComment
  ]


genSingleLineComment :: Gen Text
genSingleLineComment =
  ((<> "\n") . ("--" <>)) <$>
  Gen.text (Range.linear 0 1000)
    (Gen.element [ c | c <- ['\0'..'\xff'], c /= '\r' && c /= '\n' ])


genMultiLineComment :: Gen Text
genMultiLineComment =
  ((<> "-}") . ("{-" <>)) <$>
  Gen.text (Range.linear 0 1000)
    (Gen.element [ c | c <- ['\0'..'\xff'], c /= '}' ])


tests :: TestTree
tests =
  testGroup "tokenize"
  [ commutesWithConcatByWhitespaceTest
  , prependWhitespaceTest
  , appendWhitespaceTest
  ]


commutesWithConcatByWhitespaceTest :: TestTree
commutesWithConcatByWhitespaceTest =
  testProperty "commutes with concatenating strings separated by whitespace" . property $ do
    t0 <- forAll genText
    t1 <- forAll genText
    w  <- forAll genWhitespace
    liftA2 (<>) (f t0) (f t1) === f (t0 <> w <> t1)
    return ()
  where f = eitherToMaybe . tokenize


prependWhitespaceTest :: TestTree
prependWhitespaceTest =
  testProperty "result is unaffected by prepending whitespace" . property $ do
    t <- forAll genText
    w <- forAll genWhitespace
    eitherToMaybe (tokenize t) === eitherToMaybe (tokenize (w <> t))


appendWhitespaceTest :: TestTree
appendWhitespaceTest =
  testProperty "result is unaffected by appending whitespace" . property $ do
    t <- forAll genText
    w <- forAll genWhitespace
    eitherToMaybe (tokenize t) === eitherToMaybe (tokenize (w <> t))
