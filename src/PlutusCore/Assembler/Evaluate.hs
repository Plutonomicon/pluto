{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module PlutusCore.Assembler.Evaluate
  ( eval
  , evalToplevelBinding
  , evalToplevelBindingToHaskellValueMust
  ) where

import           Control.Monad.Except
import           Plutus.V1.Ledger.Scripts                 (Script)
import qualified Plutus.V1.Ledger.Scripts                 as Scripts
import           PlutusCore.Assembler.App
import qualified PlutusCore.Assembler.Assemble            as Assemble
import qualified PlutusCore.Assembler.Haskell             as H
import           PlutusCore.Assembler.Prelude
import qualified PlutusCore.Assembler.Types.AST           as AST
import           PlutusCore.Evaluation.Machine.ExBudget   (ExBudget)
import           PlutusTx.Evaluation                      (evaluateCekTrace)
import           UntypedPlutusCore                        (DefaultFun,
                                                           DefaultUni, Name,
                                                           Term)
import qualified UntypedPlutusCore.Evaluation.Machine.Cek as UPLC

eval :: Script -> Either Scripts.ScriptError (ExBudget, [Text], Term Name DefaultUni DefaultFun ())
eval = evaluateScript @(Either Scripts.ScriptError)

-- | Like `evalTopLevelBinding`, but expects the result to be a Haskell value.
evalToplevelBindingToHaskellValueMust :: (H.FromUPLC a, HasCallStack) => AST.Name -> [AST.Term ()] -> AST.Program () -> a
evalToplevelBindingToHaskellValueMust name args prog =
  processResult $ evalToplevelBinding name args prog
  where
    processResult :: (H.FromUPLC a, HasCallStack) => Either Error (Term Name DefaultUni DefaultFun ()) -> a
    processResult = \case
      Left err ->
        error $ show err
      Right t ->
        case H.fromUPLC t of
          Nothing -> error "processResult: failed to convert term"
          Just x  -> x

evalToplevelBinding :: AST.Name -> [AST.Term ()] -> AST.Program () -> Either Error (Term Name DefaultUni DefaultFun ())
evalToplevelBinding name args prog = do
  prog' <- liftError ErrorOther $ applyToplevelBinding name args prog
  (_, _, res) <-
    liftError ErrorEvaluating . eval
      =<< liftError ErrorAssembling (Assemble.translate prog')
  pure res

-- | Return a new program that applies a bound lambda with the given arguments
--
-- The lambda must be bound in a top-level Let binding. When there are no
-- arguments, the bound variable's value is returned as is.
applyToplevelBinding ::
  -- | Variable name of the bound term (usually a lambda)
  AST.Name ->
  -- | Arguments to apply the lambda with
  --
  -- If there are no arguments, the bound term is returned as is. If there are
  -- arguments, the bound term must be a lambda.
  [AST.Term ()] ->
  -- | The program with the let binding.
  --
  -- The Let body of this program will be discarded.
  AST.Program () ->
  -- | The new program that retains the let bindings, but with a new body
  -- containing the expression requested.
  Either Text (AST.Program ())
applyToplevelBinding name args = \case
  AST.Program (AST.Let ann bindings _oldBody) ->
    case getBoundTerm name `mapMaybe` bindings of
      [_boundTerm] -> do
        let newBody = foldl' (AST.Apply ()) (AST.Var () name) args
        pure $ AST.Program $ AST.Let ann bindings newBody
      _ -> throwError $ "expected a binding with name: " <> AST.getName name
  _ ->
    throwError "expected top-level let binding"
  where
    getBoundTerm k (AST.Binding _ k' val) = do
      guard $ k == k'
      pure val


-- | Evaluate a script, returning the trace log and term result.
--
-- This is same as `Plutus.V1.Ledger.Scripts.evaluateScript`, but returns the script result as well.
evaluateScript
 :: forall m uni fun . (MonadError Scripts.ScriptError m, uni ~ DefaultUni, fun ~ DefaultFun)
 => Script
 -> m (ExBudget, [Text], Term Name uni fun ())
evaluateScript s = do
  p <- case Scripts.mkTermToEvaluate s of
    Right p -> pure p
    Left e  -> throwError $ Scripts.MalformedScript $ show e
  let (logOut, UPLC.TallyingSt _ budget, result) = evaluateCekTrace p
  term <- case result of
    Right term -> pure term
    Left errWithCause@(UPLC.ErrorWithCause err cause) ->
      throwError $ case err of
        UPLC.InternalEvaluationError internalEvalError ->
          Scripts.EvaluationException (show errWithCause) (show internalEvalError)
        UPLC.UserEvaluationError evalError ->
          -- We use `show` here because plutus doesn't expose mkError
          Scripts.EvaluationError logOut (show (evalError, cause))
  pure (budget, logOut, term)
