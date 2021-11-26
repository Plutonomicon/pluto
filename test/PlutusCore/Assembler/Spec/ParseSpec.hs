{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}


module PlutusCore.Assembler.Spec.ParseSpec ( tests ) where


import Text.Parsec.Pos (SourcePos, newPos)

import PlutusCore.Assembler.Spec.Prelude
import PlutusCore.Assembler.Spec.Gen (genTerm)
import PlutusCore.Assembler.Parse (parse)
import PlutusCore.Assembler.Types.AST (Program (..))


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
    (t, tts) <- forAll genTerm
    parse ((,fakeSourcePos) <$> tts) === Right (Program t)
