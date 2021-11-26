{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Spec.ParseSpec ( tests ) where


import Text.Parsec.Pos (newPos)

import PlutusCore.Assembler.Spec.Prelude


tests :: TestTree
tests =
  testGroup "parse"
  [ testParseValidTokenList
  ]


fakeSourcePos :: SourcePos
fakeSourcePos = newPos "test" 0 0


testParseValidTokenList :: TestTree
testParseValidTokenList =
  testProperty "parses a syntactically valid token list" $ do
    (t, tts) <- genTerm
    parse t === Right tts
