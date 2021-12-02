{-# LANGUAGE NoImplicitPrelude #-}


module Main ( main ) where


import           Test.Tasty                                (defaultMain,
                                                            localOption)
import           Test.Tasty.Hedgehog                       (HedgehogTestLimit (..))

import           PlutusCore.Assembler.Prelude
import qualified PlutusCore.Assembler.Spec.AnnDeBruijnSpec as AnnDeBruijn
import qualified PlutusCore.Assembler.Spec.ParseSpec       as Parse
import           PlutusCore.Assembler.Spec.Prelude
import qualified PlutusCore.Assembler.Spec.Shrink          as Shrink
import qualified PlutusCore.Assembler.Spec.TokenizeSpec    as Tokenize


main :: IO ()
main = defaultMain tests


-- Number of successful tests for each Hedgehog property.
limit :: HedgehogTestLimit
limit = HedgehogTestLimit (Just 1000)


tests :: TestTree
tests =
  localOption limit
  $
  testGroup "pluto"
  [ AnnDeBruijn.tests
  , Parse.tests
  , Tokenize.tests
  , Shrink.tests
  ]
