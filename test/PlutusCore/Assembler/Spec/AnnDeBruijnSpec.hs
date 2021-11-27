{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Spec.AnnDeBruijnSpec (tests) where


import PlutusCore.Assembler.Spec.Prelude
import PlutusCore.Assembler.Prelude
import PlutusCore.Assembler.Spec.Gen (genTerm, genRecursionDepth)
import qualified PlutusCore.Assembler.Types.AST as AST
import PlutusCore.Assembler.AnnDeBruijn (annDeBruijn)


tests :: TestTree
tests =
  testGroup "annDeBruijn"
  [ testInformationPreserving
  ]


testInformationPreserving :: TestTree
testInformationPreserving =
  testProperty "preserves the existing information" . property $ do
    n <- forAll genRecursionDepth
    (t, _ts) <- forAll (genTerm n)
    (fst <$> annDeBruijn (AST.Program t)) === AST.Program t
