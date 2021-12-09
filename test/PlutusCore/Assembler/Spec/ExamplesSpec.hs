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
import qualified PlutusCore.Data                   as PLC
import           PlutusTx                          (toBuiltinData)
import           PlutusTx.Builtins                 (BuiltinData)
import           Prelude                           (Foldable (sum), head, tail,
                                                    toInteger)

-- FFIs must be declared before tests
hello :: AST.Program ()
hello = $(FFI.load "examples/hello.pluto")
$(FFI.bind 'hello
  "defaultGreeting" [t|String|])
$(FFI.bind 'hello
  "greet" [t|String -> String -> String|])


sumProg :: AST.Program ()
sumProg = $(FFI.load "examples/sum.pluto")
$(FFI.bind 'sumProg
  "sumIntList" [t|[Integer] -> Integer|])

fibProg :: AST.Program ()
fibProg = $(FFI.load "examples/fibonacci.pluto")
$(FFI.bind 'fibProg
  "fibonacciSeries" [t|BuiltinData -> PLC.Data|])

tests :: TestTree
tests =
  testGroup
    "examples"
    [ helloTest
    , sumTest
    , fibTest
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

sumTest :: TestTree
sumTest =
  testGroup
    "sum.pluto"
    [ testProperty "can sum integers" . property $ do
        xs <- fmap (fmap toInteger) $ forAll $ Gen.list (Range.linear 0 15) (Gen.int (Range.linear 0 100))
        sumIntList xs === sum xs
    ]

fibTest :: TestTree
fibTest =
  testGroup
    "fibonacci.pluto"
    [ testProperty "fib works" . property $ do
        n <- fmap toInteger $ forAll $ Gen.int (Range.linear 0 100)
        let ans = foldl' (\acc _ -> (head acc + head (tail acc)) : acc) [1, 0] [1..(n-1)]
        fibonacciSeries (toBuiltinData n) === PLC.List (PLC.I <$> ans)
    ]
