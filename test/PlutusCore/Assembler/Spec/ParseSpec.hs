{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}


module PlutusCore.Assembler.Spec.ParseSpec ( tests ) where


import qualified Hedgehog.Gen                      as Gen
import qualified Hedgehog.Range                    as Range
import           Text.Parsec.Pos                   (SourcePos, newPos)

import           PlutusCore.Assembler.Parse        (parse)
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Spec.Gen     (genTerm)
import           PlutusCore.Assembler.Spec.Prelude
import           PlutusCore.Assembler.Types.AST    (Program (..))


tests :: TestTree
tests =
  testGroup "parse"
  [ testParseValidTokenList
  ]


fakeSourcePos :: SourcePos
fakeSourcePos = newPos "test" 0 0


testParseValidTokenList :: TestTree
testParseValidTokenList =
  testProperty "parses a syntactically valid token list" . property $ do
    n <- forAll (Gen.integral (Range.linear 0 10))
    (t, tts) <- forAll (genTerm n)
    parse ((,fakeSourcePos) <$> tts) === Right (Program t)
