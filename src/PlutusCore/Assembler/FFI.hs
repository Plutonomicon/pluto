{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module PlutusCore.Assembler.FFI
  ( load
  , bind
  ) where

import           Data.Data                      (Data, cast)
import qualified Data.Text                      as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax     (Lift (lift), dataToExpQ)
import           PlutusCore.Assembler.App       (Error (ErrorParsing))
import qualified PlutusCore.Assembler.Assemble  as Assemble
import qualified PlutusCore.Assembler.Evaluate  as E
import qualified PlutusCore.Assembler.Haskell   as H
import           PlutusCore.Assembler.Prelude
import qualified PlutusCore.Assembler.Types.AST as AST
import           System.IO                      (FilePath)

-- | Embed the AST of a Pluto program into the current module.
load :: FilePath -> Q Exp
load =
  liftDataWithText <=< loadPlutoMod
  where
    loadPlutoMod :: (MonadIO m, MonadFail m) => FilePath -> m (AST.Program ())
    loadPlutoMod fp = do
      s <- liftIO $ readFile fp
      either (fail . show . ErrorParsing) (pure . void)
        $ Assemble.parseProgram fp (T.pack s)

-- | Import a top-level bound value (or function) from a Pluto program.
bind ::
  -- | The pluto program to import from
  Name ->
  -- | The variable name of the top-level binding to import.
  String ->
  -- | Expected value type.
  Q Type ->
  Q [Dec]
bind prog name qType = do
  type_ <- qType
  simpleD name type_ $ \args ->
    [|
      E.evalToplevelBindingToHaskellValueMust
        (fromString name)
        $( listE $
              flip fmap args $ \arg -> do
                [|H.toPluto|] `appE` varE arg
          )
        $(varE prog)
      |]

-- TH Utilities

-- | A simple value or function declaration
--
-- Type must be a simple type or an arrow type.
simpleD ::
  -- | The name of the declaration
  String ->
  -- | The type of the declaration
  Type ->
  -- | The body of the declaration, as a function taking the arguments if any
  ([Name] -> Q Exp) ->
  Q [Dec]
simpleD name type_ bodyF = do
  args <- lambdaArgsFromType type_
  body <- NormalB <$> bodyF args
  let sym = mkName name
  pure
    [ SigD sym type_,
      if null args
        then ValD (VarP sym) body []
        else FunD sym [Clause (VarP <$> args) body []]
    ]
  where
    lambdaArgsFromType :: Type -> Q [Name]
    lambdaArgsFromType = \case
      ConT _ -> do
        pure []
      AppT (AppT ArrowT (ConT _)) rest -> do
        n <- newName "_arg"
        (n :) <$> lambdaArgsFromType rest
      _ ->
        fail "simpleD: unexpected type"

-- | Like `liftData` but `Data.Text`-friendly.
--
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/12596#note_169275
liftDataWithText :: Data a => a -> Q Exp
liftDataWithText =
  dataToExpQ (fmap liftText . cast)
  where
    liftText :: T.Text -> Q Exp
    liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)
