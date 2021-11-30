{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}

module PlutusCore.Assembler.Evaluate (eval) where

import           Plutus.V1.Ledger.Scripts               (Script)
import qualified Plutus.V1.Ledger.Scripts               as Scripts
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Evaluation.Machine.ExBudget (ExBudget)

eval :: Script -> Either Scripts.ScriptError (ExBudget, [Text])
eval = Scripts.evaluateScript @(Either Scripts.ScriptError)
