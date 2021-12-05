{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module PlutusCore.Assembler.Spec.ExamplesSpec (tests) where

import           Data.Function
import qualified Hedgehog.Gen                      as Gen
import qualified Hedgehog.Range                    as Range
import qualified PlutusCore.Assembler.FFI          as FFI
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Spec.Prelude
import qualified PlutusCore.Assembler.Types.AST    as AST

-- FFIs must be declared before tests
hello :: AST.Program ()
hello = $(FFI.load "examples/hello.pluto")
$(FFI.bind 'hello
  "defaultGreeting" [t|String|])
$(FFI.bind 'hello
  "greet" [t|String -> String -> String|])

type UplcRational = (Integer, Integer)

rationals :: AST.Program ()
rationals = $(FFI.load "examples/field-of-rationals.pluto")
$(FFI.bind 'rationals "plus" [t|UplcRational -> UplcRational -> UplcRational|])

tests :: TestTree
tests =
  testGroup
    "examples"
    [ helloTest
    , fieldOfRationalsTest
    ]

helloTest :: TestTree
helloTest =
  testGroup
    "hello.pluto"
    [ testProperty "constructs greeting correctly regardless of input" . property $ do
        greeting <- forAll someText
        name <- forAll someText
        greet greeting name === greeting <> ", " <> name
    , testProperty "default greeting is Hello" . property $ do
        name <- forAll someText
        defaultGreeting === "Hello"
        greet defaultGreeting name === "Hello" <> ", " <> name
    ]
  where
    someText = Gen.string (Range.linear 3 9) Gen.alpha

fieldOfRationalsTest :: TestTree
fieldOfRationalsTest =
  testGroup "field-of-rationals.pluto"
  [ testProperty "addition is commutative" . property $ do
      return ()
  ]
{-
  where
    someRatio = (,) <$> 
-}
