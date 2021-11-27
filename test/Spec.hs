{-# LANGUAGE NoImplicitPrelude #-}


module Main ( main ) where


import           Test.Tasty                             (defaultMain,
                                                         localOption)
import           Test.Tasty.Hedgehog                    (HedgehogTestLimit (..))

import PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Spec.Prelude
import qualified PlutusCore.Assembler.Spec.ParseSpec    as Parse
import qualified PlutusCore.Assembler.Spec.TokenizeSpec as Tokenize


main :: IO ()
main = defaultMain tests


-- Number of successful tests for each Hedgehog property.
limit :: HedgehogTestLimit
limit = HedgehogTestLimit (Just 1000)


tests :: TestTree
tests =
  localOption limit
  $
  testGroup "plutus-core-assembler"
  [ Parse.tests
  , Tokenize.tests
  ]
