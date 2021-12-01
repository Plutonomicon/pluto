{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PlutusCore.Assembler.Spec.ExamplesSpec (tests) where

import           Control.Monad.Catch               (MonadThrow, throwM)
import           Data.Function
import           Data.Text.IO                      (readFile)
import qualified Hedgehog.Gen                      as Gen
import qualified Hedgehog.Range                    as Range
import qualified PlutusCore                        as PLC
import           PlutusCore.Assembler.App
import qualified PlutusCore.Assembler.Assemble     as Assemble
import qualified PlutusCore.Assembler.Build        as B
import qualified PlutusCore.Assembler.Evaluate     as E
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Spec.Prelude
import qualified PlutusCore.Assembler.Types.AST    as AST
import           System.FilePath                   (FilePath)
import qualified UntypedPlutusCore                 as UPLC

tests :: TestTree
tests =
  testGroup
    "examples"
    [ helloTest
    ]

helloTest :: TestTree
helloTest =
  testGroup
    "hello.pluto"
    [ testProperty "accepts diverse greetings" . property $ do
        -- The script itself
        helloProg <- loadPlutoMod "examples/hello.pluto"
        -- Arguments to the 'greet' function
        greeting <- forAll someText
        name <- forAll someText
        -- Call 'greet' function with the arguments, and compare
        (helloProg
          & E.evalToplevelBinding "greet" [B.text greeting, B.text name]
          & fmap disassembleString)
          === Right (Just $ greeting <> ", " <> name)
    ]
  where
    someText = Gen.text (Range.linear 3 9) Gen.alpha

disassembleString :: UPLC.Term name PLC.DefaultUni fun () -> Maybe Text
disassembleString = \case
  (UPLC.Constant () (PLC.Some (PLC.ValueOf PLC.DefaultUniString x))) -> pure x
  _                                                                  -> Nothing

loadPlutoMod :: (MonadIO m, MonadThrow m) => FilePath -> m (AST.Program ())
loadPlutoMod fp = do
  s <- liftIO $ readFile fp
  either (throwM . ErrorParsing) (pure . void) $ Assemble.parseProgram "<test>" s
