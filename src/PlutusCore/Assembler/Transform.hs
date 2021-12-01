{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Functions that query and transform the Pluto AST in various ways
module PlutusCore.Assembler.Transform
  ( applyToplevelBinding,
  )
where

import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Types.AST

-- | Return a new program that applies a bound lambda with the given arguments
--
-- The lambda must be bound in a top-level Let binding. When there are no
-- arguments, the bound variable's value is returned as is.
applyToplevelBinding ::
  -- | Variable name of the bound term (usually a lambda)
  Name ->
  -- | Arguments to apply the lambda with
  --
  -- If there are no arguments, the bound term is returned as is. If there are
  -- arguments, the bound term must be a lambda.
  [Term ()] ->
  -- | The program with the let binding.
  --
  -- The Let body of this program will be discarded.
  Program () ->
  -- | The new program that retains the let bindings, but with a new body
  -- containing the expression requested.
  Either Text (Program ())
applyToplevelBinding var args = \case
  Program (Let ann bindings _oldBody) ->
    case getBoundLambda var `mapMaybe` bindings of
      [_boundTerm] -> do
        let newBody = foldl' (Apply ()) (Var () var) args
        pure $ Program $ Let ann bindings newBody
      _ -> throwError $ "expected a binding with name: " <> getName var
  _ ->
    throwError "expected top-level let binding"
  where
    getBoundLambda k (Binding _ k' val) = do
      guard $ k == k'
      pure val
