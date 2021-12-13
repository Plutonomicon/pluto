{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
module Sample.Validator.Haskell (haskellValidator) where

import           Ledger           (Validator, mkValidatorScript)
import qualified PlutusTx
import           PlutusTx.Prelude


-- -------------------------------------------------------------------------- --
-- Onchain (Haskell)                                                          --
-- -------------------------------------------------------------------------- --

-- | The number the gift receiver must guess to redeem the gift.
--
-- Note: This is purposefully insecure, because we are writing a validator that
-- does not operate on ScriptContext, yet. Later, once we get ScriptContext
-- access working in Pluto, we will change this.
type Guess = Integer

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator
  (PlutusTx.unsafeFromBuiltinData -> correctGuess)
  (PlutusTx.unsafeFromBuiltinData -> guess)
  _scriptCtx =
    if (guess :: Guess) == correctGuess
    then ()
    else error ()

haskellValidator :: Validator
haskellValidator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

