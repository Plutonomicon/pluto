{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
module Main where

import           Control.Monad                 hiding (fmap)
import qualified Control.Monad.Freer.Extras    as Extras
import           Data.Map                      as Map
import           Data.Text                     (Text)
import           Data.Void                     (Void)
import           Ledger                        hiding (singleton)
import           Ledger.Ada                    as Ada
import           Ledger.Constraints            as Constraints
import qualified Ledger.Scripts                as Scripts
import           Plutus.Contract
import           Plutus.Contract.Test          (w1, w2)
import           Plutus.Trace.Emulator         (EmulatorTrace)
import qualified Plutus.Trace.Emulator         as Em
import qualified PlutusCore.Assembler.Assemble as Pluto
import qualified PlutusCore.Assembler.FFI      as PlutoFFI
import           PlutusTx                      (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude              hiding (Semigroup (..), unless)
import           Prelude                       (IO, Semigroup (..), String)
import qualified Prelude
import           Text.Printf                   (printf)
import           Wallet.Emulator.Wallet

-- -------------------------------------------------------------------------- --
-- Onchain (Pluto)                                                            --
-- -------------------------------------------------------------------------- --

plutoValidatorProg = $(PlutoFFI.load "examples/contracts/sample/validator.pluto")

plutoValidator :: Validator
plutoValidator =
  case Pluto.translate plutoValidatorProg of
    Left err     -> Prelude.error $ Prelude.show err
    Right script -> Validator script

-- -------------------------------------------------------------------------- --
-- Onchain (Haskell)                                                          --
-- -------------------------------------------------------------------------- --

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()

haskellValidator :: Validator
haskellValidator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

-- -------------------------------------------------------------------------- --
-- Offchain code                                                              --
-- -------------------------------------------------------------------------- --

-- Selecting validator from the above
validator :: Validator
validator = plutoValidator

type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" ()

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript (Scripts.validatorHash validator) (Datum $ PlutusTx.dataToBuiltinData $ List []) $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    utxos <- utxosAt (scriptAddress validator)
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.dataToBuiltinData $ I 17 | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints =
  forever $ selectList [
    endpoint @"give" give,
    endpoint @"grab" (\() -> grab)
  ]

main :: IO ()
main =
  Em.runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace ()
myTrace = do
  h1 <- Em.activateContractWallet w1 endpoints
  h2 <- Em.activateContractWallet w2 endpoints
  Em.callEndpoint @"give" h1 (10*1000000)
  void $ Em.waitUntilSlot 1
  Em.callEndpoint @"grab" h2 ()
  s <- Em.waitNSlots 1
  Extras.logInfo "done"
