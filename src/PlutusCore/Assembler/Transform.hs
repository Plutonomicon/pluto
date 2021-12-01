{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Functions that query and transform the Pluto AST in various ways
module PlutusCore.Assembler.Transform
  ( applyToplevelBoundLambdaWith,
  )
where

import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Types.AST

-- | Return a new program that applies a bound lambda with the given argument
--
-- The lambda must be bound in a top-level Let binding.
applyToplevelBoundLambdaWith ::
  -- | Variable name of the bound lambda
  Name ->
  -- | Argument to apply the lambda with
  Term () ->
  -- | The program with the let binding.
  --
  -- The Let body of this program will be discarded.
  Program () ->
  -- | The new program that retains the let bindings, but with a new body that
  -- applies the lambda.
  Either Text (Program ())
applyToplevelBoundLambdaWith var arg = \case
  Program (Let ann bindings _oldBody) ->
    case getBoundLambda var `mapMaybe` bindings of
      [_lambdaTerm] ->
        -- TODO: Use lambdaTerm to process nested lambdas, for handling n-arity
        -- functions with n>1
        pure $
          Program $
            Let ann bindings $
              Apply () (Var () var) arg
      _ -> throwError $ "expected a lambda bound with name: " <> getName var
  _ ->
    throwError "expected top-level let binding"
  where
    getBoundLambda k (Binding _ k' val) = do
      guard $ k == k'
      case val of
        (Lambda _ _ term) -> pure term
        _                 -> Nothing
