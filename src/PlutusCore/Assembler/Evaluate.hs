{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module PlutusCore.Assembler.Evaluate
  ( eval
  , evalToplevelBinding
  ) where

import           Control.Monad.Except
import           Plutus.V1.Ledger.Scripts                 (Script)
import qualified Plutus.V1.Ledger.Scripts                 as Scripts
import           PlutusCore.Assembler.App
import qualified PlutusCore.Assembler.Assemble            as Assemble
import qualified PlutusCore.Assembler.Build               as Build
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

evalToplevelBinding :: AST.Name -> [AST.Term ()] -> AST.Program () -> Either Error (Term Name DefaultUni DefaultFun ())
evalToplevelBinding name args prog = do
  prog' <- liftError ErrorOther $ Build.applyToplevelBinding name args prog
  (_, _, res) <-
    liftError ErrorEvaluating . eval
      =<< liftError ErrorAssembling (Assemble.translate prog')
  pure res

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
