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
genWhitespace = Gen.text (Range.singleton 1) (Gen.element [' ', '\r', '\n'])


tests :: TestTree
tests =
  testGroup "tokenize"
  [ commutesWithConcatByNewlineTest
  , prependWhitespaceTest
  , appendWhitespaceTest
  ]


commutesWithConcatByNewlineTest :: TestTree
commutesWithConcatByNewlineTest =
  testProperty "commutes with concatenating strings separated by a newline" . property $ do
    t0 <- forAll genText
    t1 <- forAll genText
    liftA2 (<>) (f t0) (f t1) === f (t0 <> "\n" <> t1)
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
