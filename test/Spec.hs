{-# LANGUAGE NoImplicitPrelude #-}


module Main ( main, limit, tests ) where


import           Test.Tasty                                (defaultMain,
                                                            localOption)
import           Test.Tasty.Hedgehog                       (HedgehogTestLimit (..))

import           PlutusCore.Assembler.Prelude
import qualified PlutusCore.Assembler.Spec.AnnDeBruijnSpec as AnnDeBruijn
import qualified PlutusCore.Assembler.Spec.ExamplesSpec    as Examples
import qualified PlutusCore.Assembler.Spec.ParseSpec       as Parse
import           PlutusCore.Assembler.Spec.Prelude
import qualified PlutusCore.Assembler.Spec.Shrink          as Shrink
import qualified PlutusCore.Assembler.Spec.TokenizeSpec    as Tokenize


main :: IO ()
main = do
  shrinkTests <- Shrink.makeTests
  defaultMain (tests shrinkTests)


-- Number of successful tests for each Hedgehog property.
limit :: HedgehogTestLimit
limit = HedgehogTestLimit (Just 1000)


tests :: TestTree -> TestTree
tests shrinkTests =
  localOption limit $
    testGroup "main"
      [ testGroup "pluto"
          [ AnnDeBruijn.tests
          , Parse.tests
          , Tokenize.tests
          , shrinkTests
          ]
      , testGroup "example"
          [ Examples.tests
          ]
      ]
