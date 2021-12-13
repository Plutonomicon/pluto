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
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Scripts       as Scripts
import           Plutus.Contract
import qualified Plutus.V1.Ledger.Api as PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup (..), unless)
import qualified Prelude
import qualified Text.Printf          as Printf

-- -------------------------------------------------------------------------- --
-- Offchain code                                                              --
-- -------------------------------------------------------------------------- --

type GiftSchema =
            Endpoint "give" (PubKeyHash, Integer)
        .\/ Endpoint "grab" ()

give :: AsContractError e => Validator -> (PubKeyHash, Integer) -> Contract w s e ()
give validator (beneficiary, amount) = do
    let tx =
          Constraints.mustPayToOtherScript
            (Scripts.validatorHash validator)
            (Datum $ PlutusTx.toBuiltinData beneficiary)
            (Ada.lovelaceValueOf amount)
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @Prelude.String $ Printf.printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Validator -> Contract w s e ()
grab validator = do
    utxos <- utxosAt (scriptAddress validator)
    pkh <- ownPubKeyHash
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      Prelude.<>
                  Constraints.otherScript validator
        tx :: Constraints.TxConstraints Void Void
        tx      = mconcat [ Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData () | oref <- orefs ]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    logInfo @Prelude.String $ Prelude.show ledgerTx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @Prelude.String $ "collected gifts: " Prelude.<> Prelude.show pkh

endpoints :: Validator -> Contract () GiftSchema Text ()
endpoints v =
  forever $ selectList [
    endpoint @"give" (give v),
    endpoint @"grab" (const $ grab v)
  ]
