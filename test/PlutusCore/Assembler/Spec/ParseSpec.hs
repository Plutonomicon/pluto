{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}


module PlutusCore.Assembler.Spec.ParseSpec ( tests ) where


import           Text.Parsec.Pos                   (SourcePos, newPos)

import           PlutusCore.Assembler.Parse        (parse)
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Spec.Gen     (genTerm, genRecursionDepth)
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
    n <- forAll genRecursionDepth
    (t, tts) <- forAll (genTerm n)
    (const () <$$> parse ((,fakeSourcePos) <$> tts)) === Right (Program t)
