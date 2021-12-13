{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
module Sample.Validator.Haskell (haskellValidator) where

import           Ledger           (Validator, mkValidatorScript)
import           Ledger.Contexts  (ScriptContext (scriptContextTxInfo),
                                   txSignedBy)
import qualified PlutusTx
import           PlutusTx.Prelude


-- -------------------------------------------------------------------------- --
-- Onchain (Haskell)                                                          --
-- -------------------------------------------------------------------------- --

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator
  (PlutusTx.unsafeFromBuiltinData -> pkh)
  _
  (PlutusTx.unsafeFromBuiltinData -> ctx) =
    if traceIfFalse "beneficiary's signature missing" (scriptContextTxInfo ctx `txSignedBy` pkh)
    then ()
    else error ()

haskellValidator :: Validator
haskellValidator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

