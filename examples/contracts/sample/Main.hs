{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# LANGUAGE ViewPatterns     #-}
module Main where

import           Control.Monad                  hiding (fmap)
import qualified Control.Monad.Freer.Extras     as Extras
import           Data.Map                       as Map
import           Data.Text                      (Text)
import           Data.Void                      (Void)
import           Ledger                         hiding (singleton)
import           Ledger.Ada                     as Ada
import           Ledger.Constraints             as Constraints
import qualified Ledger.Scripts                 as Scripts
import           Plutus.Contract
import           Plutus.Contract.Test           (w1, w2, w3)
import           Plutus.Trace.Emulator          (EmulatorTrace)
import qualified Plutus.Trace.Emulator          as Em
import qualified Plutus.V1.Ledger.Api           as PlutusTx
import qualified PlutusCore.Assembler.Assemble  as Pluto
import qualified PlutusCore.Assembler.FFI       as PlutoFFI
import qualified PlutusCore.Assembler.Types.AST as Pluto
import           PlutusTx                       (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude               hiding (Semigroup (..), unless)
import           Prelude                        (IO, Semigroup (..), String)
import qualified Prelude
import           Text.Printf                    (printf)
import           Wallet.Emulator.Wallet

-- -------------------------------------------------------------------------- --
-- Onchain (Pluto)                                                            --
-- -------------------------------------------------------------------------- --

plutoValidatorProg :: Pluto.Program ()
plutoValidatorProg = $(PlutoFFI.load "examples/contracts/sample/validator.pluto")

plutoValidator :: Validator
plutoValidator =
  case Pluto.translate plutoValidatorProg of
    -- TODO: fail in TH
    Left err     -> Prelude.error $ Prelude.show err
    Right script -> Validator script

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
  _ =
    if (guess :: Guess) == correctGuess
    then ()
    else error ()

haskellValidator :: Validator
haskellValidator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

-- -------------------------------------------------------------------------- --
-- Offchain code                                                              --
-- -------------------------------------------------------------------------- --

-- Selecting validator from the above
validator :: Validator
validator = plutoValidator

type GiftSchema =
            Endpoint "give" (Guess, Integer)
        .\/ Endpoint "grab" Guess

give :: AsContractError e => (Guess, Integer) -> Contract w s e ()
give (correctGuess, amount) = do
    let tx =
          mustPayToOtherScript
            (Scripts.validatorHash validator)
            (Datum $ PlutusTx.toBuiltinData correctGuess)
            (Ada.lovelaceValueOf amount)
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Guess -> Contract w s e ()
grab guess = do
    utxos <- utxosAt (scriptAddress validator)
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData guess | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints =
  forever $ selectList [
    endpoint @"give" give,
    endpoint @"grab" grab
  ]

main :: IO ()
main =
  Em.runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace ()
myTrace = do
  h1 <- Em.activateContractWallet w1 endpoints
  h2 <- Em.activateContractWallet w2 endpoints
  h3 <- Em.activateContractWallet w3 endpoints
  Em.callEndpoint @"give" h1 (42, 10*1000000)
  void $ Em.waitUntilSlot 1
  Em.callEndpoint @"grab" h2 24 -- Incorrect guess
  s <- Em.waitNSlots 1
  Em.callEndpoint @"grab" h3 42
  s <- Em.waitNSlots 1
  Extras.logInfo "done"
