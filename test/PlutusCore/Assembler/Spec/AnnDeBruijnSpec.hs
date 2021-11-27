{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Spec.AnnDeBruijnSpec (tests) where


import           PlutusCore.DeBruijn               (DeBruijn (..), Index (..))

import           PlutusCore.Assembler.AnnDeBruijn  (annDeBruijn)
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Spec.Gen     (genRecursionDepth, genTerm)
import           PlutusCore.Assembler.Spec.Prelude
import qualified PlutusCore.Assembler.Types.AST    as AST


tests :: TestTree
tests =
  testGroup "annDeBruijn"
  [ testInformationPreserving
  , testNonNegative
  ]


testInformationPreserving :: TestTree
testInformationPreserving =
  testProperty "preserves the existing information" . property $ do
    n <- forAll genRecursionDepth
    (t, _ts) <- forAll (genTerm n)
    (fst <$> annDeBruijn (AST.Program t)) === AST.Program t


testNonNegative :: TestTree
testNonNegative =
  testProperty "de Bruijn indices are non-negative" . property $ do
    n <- forAll genRecursionDepth
    (t, _ts) <- forAll (genTerm n)
    assert $ all (all isNonNeg . snd) (annDeBruijn (AST.Program t))
  where
    isNonNeg (DeBruijn (Index n)) = n >= 0
