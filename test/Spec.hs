{-# LANGUAGE NoImplicitPrelude #-}


module Main ( main ) where


import Test.Tasty (defaultMain)

import PlutusCore.Assembler.Spec.Prelude
import qualified PlutusCore.Assembler.Spec.TokenizeSpec as Tokenize


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests =
  testGroup "plutus-core-assembler"
  [ Tokenize.tests
  ]
