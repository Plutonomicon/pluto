{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns       #-}
module Main where

import qualified Control.Applicative                as App
import           Control.Lens.Combinators
import           Control.Lens.Operators
import           Control.Lens.TH
import           Control.Monad                      hiding (fmap)
import qualified Control.Monad.Freer.Extras         as Extras
import           Data.Map                           as Map
import qualified Data.Map.Strict                    as Map
import           Data.Text                          (Text)
import           Data.Void                          (Void)
import qualified Hedgehog                           as H
import qualified Hedgehog.Main                      as H
import           Ledger                             hiding (singleton)
import           Ledger.Ada                         as Ada
import           Ledger.Constraints                 as Constraints
import qualified Ledger.Scripts                     as Scripts
import           Plutus.Contract
import           Plutus.Contract.Test               (w1, w2, w3)
import qualified Plutus.Contract.Test               as PCT
import           Plutus.Contract.Test.ContractModel (($=))
import qualified Plutus.Contract.Test.ContractModel as PCT
import           Plutus.Trace.Emulator              (EmulatorTrace)
import qualified Plutus.Trace.Emulator              as Em
import qualified Plutus.V1.Ledger.Api               as PlutusTx
import qualified PlutusCore.Assembler.Assemble      as Pluto
import qualified PlutusCore.Assembler.FFI           as PlutoFFI
import qualified PlutusCore.Assembler.Types.AST     as Pluto
import           PlutusTx                           (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude                   hiding (Semigroup (..),
                                                     unless)
import           Prelude                            (IO, Semigroup (..), String)
import qualified Prelude
import qualified Test.QuickCheck.Gen                as QC
import qualified Test.QuickCheck.Property           as QC
import qualified Test.Tasty                         as T
import qualified Test.Tasty.Hedgehog                as H
import qualified Test.Tasty.QuickCheck              as QC
import           Text.Printf                        (printf)
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
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Validator -> Guess -> Contract w s e ()
grab validator guess = do
    utxos <- utxosAt (scriptAddress validator)
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData guess | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Validator -> Contract () GiftSchema Text ()
endpoints v =
  forever $ selectList [
    endpoint @"give" (give v),
    endpoint @"grab" (grab v)
  ]

-- -------------------------------------------------------------------------- --
-- Tests                                                                      --
-- -------------------------------------------------------------------------- --

newtype SampleModel = SampleModel { _giftAmount :: Integer }
  deriving stock (Prelude.Eq, Prelude.Show)

makeLenses ''SampleModel

genWallet :: QC.Gen Wallet
genWallet = QC.elements knownWallets

testGuess :: Guess
testGuess = 42

instance PCT.ContractModel SampleModel where
  data Action SampleModel = Give Wallet Guess Integer | Grab Wallet Guess
    deriving stock (Prelude.Show, Prelude.Eq)

  data ContractInstanceKey SampleModel _ _ _ where
    UseContract :: Wallet -> PCT.ContractInstanceKey SampleModel () GiftSchema Text

  arbitraryAction :: PCT.ModelState SampleModel -> QC.Gen (PCT.Action SampleModel)
  arbitraryAction _ = do
    wallet <- genWallet
    let
      minAda = 2
      value = (1000000 *) Prelude.<$> QC.choose @Integer (minAda, 5)
    QC.oneof [
      Prelude.fmap (Give wallet testGuess) value,
      Prelude.pure $ Grab wallet testGuess
      ]

  initialState :: SampleModel
  initialState = SampleModel 0

  precondition :: PCT.ModelState SampleModel -> PCT.Action SampleModel -> Bool
  precondition s (Give w _ _) = s ^. PCT.contractState . giftAmount Prelude.>= 0
  precondition s (Grab w g)   = s ^. PCT.contractState . giftAmount Prelude.>= 0

  nextState :: PCT.Action SampleModel -> PCT.Spec SampleModel ()
  nextState = \case
    Give w g v -> do
      PCT.withdraw w (lovelaceValueOf v)
      giftAmount PCT.$~ (Prelude.+ v)
      PCT.wait 1
    Grab w _ -> do
      v <- PCT.askContractState (view giftAmount)
      PCT.deposit w (lovelaceValueOf v)
      giftAmount PCT.$= 0
      PCT.wait 1

  perform :: PCT.HandleFun SampleModel -> PCT.ModelState SampleModel -> PCT.Action SampleModel -> EmulatorTrace ()
  perform h _ = \case
    Give w g n -> do
      Em.callEndpoint @"give" (h $ UseContract w) (g, n)
      void $ Em.waitNSlots 1
    Grab w g -> do
      Em.callEndpoint @"grab" (h $ UseContract w) g
      void $ Em.waitNSlots 1

deriving stock instance Prelude.Show (PCT.ContractInstanceKey SampleModel w s e)
deriving stock instance Prelude.Eq (PCT.ContractInstanceKey SampleModel w s e)


instanceSpec :: Validator -> [PCT.ContractInstanceSpec SampleModel]
instanceSpec vl = [PCT.ContractInstanceSpec (UseContract w) w (endpoints vl) | w <- knownWallets]

modelCheck :: Validator -> PCT.Actions SampleModel -> QC.Property
modelCheck vl = QC.withMaxSuccess 10 . PCT.propRunActionsWithOptions
    PCT.defaultCheckOptions
    (instanceSpec vl)
    (Prelude.const $ Prelude.pure True)

noFundsLocked :: Validator -> QC.Property
noFundsLocked vl =
    QC.withMaxSuccess 10
      $ PCT.checkNoLockedFundsProof PCT.defaultCheckOptions (instanceSpec vl) (PCT.NoLockedFundsProof (PCT.action $ Grab w1 testGuess) (PCT.action . flip Grab testGuess))



smokeTrace :: EmulatorTrace ()
smokeTrace = do
  let ep = endpoints haskellValidator
  h1 <- Em.activateContractWallet w1 ep
  h2 <- Em.activateContractWallet w2 ep
  h3 <- Em.activateContractWallet w3 ep
  Em.callEndpoint @"give" h1 (42, 10*1000000)
  void $ Em.waitUntilSlot 1
  Em.callEndpoint @"grab" h2 24 -- Incorrect guess
  s <- Em.waitNSlots 1
  Em.callEndpoint @"grab" h3 42
  s <- Em.waitNSlots 1
  Extras.logInfo "done"

tests :: IO ()
tests = do
  T.defaultMain $
    T.localOption (H.HedgehogTestLimit (Just 10)) . T.localOption (H.HedgehogShrinkLimit (Just 2)) $
      T.testGroup "Sample Contract"
         $ flip fmap [("Haskell", haskellValidator), ("Pluto", plutoValidator)] $ \(k, validator) ->
          T.testGroup ("Validator:" <> k) [
            QC.testProperty "contract" (modelCheck validator)
            , QC.testProperty "nofundslocked" (noFundsLocked validator)
          ]

main :: IO ()
main =
  -- Em.runEmulatorTraceIO myTrace
  tests
