{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions that query and transform the Pluto AST in various ways
module PlutusCore.Assembler.Transform
  ( replaceLetBody
  , queryTopLevelBinding
  , applyVarWithString
  ) where

import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Types.AST

replaceLetBody :: Term ann -> Program ann -> Either Text (Program ann)
replaceLetBody newBody = \case
  Program (Let ann bindings _oldBody) ->
    pure $ Program (Let ann bindings newBody)
  _ ->
    throwError "expected top-level let binding"

queryTopLevelBinding :: Name -> Program ann -> Either Text (Maybe (Term ann))
queryTopLevelBinding var = \case
  Program (Let _ann bindings _body) ->
    pure $ do
      Binding _ _ term <- find (\(Binding _ name _) -> name == var) bindings
      pure term
  _ ->
    throwError "expected top-level let binding"

applyVarWithString :: Name -> Term () -> Term ()
applyVarWithString name = Apply
    ()
    (Var () name)
