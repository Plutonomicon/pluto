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


tests :: TestTree
tests =
  testGroup "tokenize"
  [ commutesWithConcatByNewline
  ]


commutesWithConcatByNewline :: TestTree
commutesWithConcatByNewline =
  testProperty "commutes with concatenating strings separated by a newline" . property $ do
    t0 <- forAll genText
    t1 <- forAll genText
    liftA2 (<>) (f t0) (f t1) === f (t0 <> "\n" <> t1)
    return ()
  where f = eitherToMaybe . tokenize
