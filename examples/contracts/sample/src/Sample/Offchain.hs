{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
module Sample.Offchain (
  endpoints,
  GiftSchema
  ) where

import           Control.Monad        (forever, void)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Ledger               hiding (singleton)
import           Ledger.Ada           as Ada (lovelaceValueOf)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Scripts       as Scripts
import           Plutus.Contract
import qualified Plutus.V1.Ledger.Api as PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup (..), unless)
import qualified Prelude
import qualified Text.Printf          as Printf

-- -------------------------------------------------------------------------- --
-- Offchain code                                                              --
-- -------------------------------------------------------------------------- --

type Guess = Integer

type GiftSchema =
            Endpoint "give" (Guess, Integer)
        .\/ Endpoint "grab" Guess

give :: AsContractError e => Validator -> (Guess, Integer) -> Contract w s e ()
give validator (correctGuess, amount) = do
    let tx =
          mustPayToOtherScript
            (Scripts.validatorHash validator)
            (Datum $ PlutusTx.toBuiltinData correctGuess)
            (Ada.lovelaceValueOf amount)
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @Prelude.String $ Printf.printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Validator -> Guess -> Contract w s e ()
grab validator guess = do
    utxos <- utxosAt (scriptAddress validator)
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      Prelude.<>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData guess | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @Prelude.String $ "collected gifts"

endpoints :: Validator -> Contract () GiftSchema Text ()
endpoints v =
  forever $ selectList [
    endpoint @"give" (give v),
    endpoint @"grab" (grab v)
  ]
