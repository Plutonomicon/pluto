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

module Main (main, tests) where

import           Control.Lens.Combinators           (makeLenses, view)
import           Control.Lens.Operators             ((^.))
import           Control.Monad                      (void)
import qualified Control.Monad.Freer.Extras         as Extras
import           Data.Text                          (Text)
import           Ledger                             hiding (singleton)
import           Ledger.Ada                         as Ada (lovelaceValueOf)
import           Plutus.Contract.Test               (w1, w2, w3)
import qualified Plutus.Contract.Test               as PCT
import qualified Plutus.Contract.Test.ContractModel as PCT
import           Plutus.Trace.Emulator              (EmulatorTrace)
import qualified Plutus.Trace.Emulator              as Em
import           Sample.Offchain
import           Sample.Validator.Haskell           (haskellValidator)
import           Sample.Validator.Pluto             (plutoValidator)
import qualified Test.QuickCheck.Gen                as QC
import qualified Test.QuickCheck.Property           as QC
import qualified Test.Tasty                         as T
import qualified Test.Tasty.Hedgehog                as H
import qualified Test.Tasty.QuickCheck              as QC
import           Wallet.Emulator.Wallet             (Wallet, knownWallets)


-- -------------------------------------------------------------------------- --
-- Tests                                                                      --
-- -------------------------------------------------------------------------- --

type Guess = Integer

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
      val = (1000000 *) Prelude.<$> QC.choose @Integer (minAda, 5)
    QC.oneof [
      Prelude.fmap (Give wallet testGuess) val,
      Prelude.pure $ Grab wallet testGuess
      ]

  initialState :: SampleModel
  initialState = SampleModel 0

  precondition :: PCT.ModelState SampleModel -> PCT.Action SampleModel -> Bool
  precondition s (Give _w _ _) = s ^. PCT.contractState . giftAmount Prelude.>= 0
  precondition s (Grab _w _g)   = s ^. PCT.contractState . giftAmount Prelude.>= 0

  nextState :: PCT.Action SampleModel -> PCT.Spec SampleModel ()
  nextState = \case
    Give w _g v -> do
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


smokeTrace :: Validator -> EmulatorTrace ()
smokeTrace validator = do
  let ep = endpoints validator
  h1 <- Em.activateContractWallet w1 ep
  h2 <- Em.activateContractWallet w2 ep
  h3 <- Em.activateContractWallet w3 ep
  Em.callEndpoint @"give" h1 (42, 10*1000000)
  void $ Em.waitUntilSlot 1
  Em.callEndpoint @"grab" h2 24 -- Incorrect guess
  _s <- Em.waitNSlots 1
  Em.callEndpoint @"grab" h3 42
  _s <- Em.waitNSlots 1
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
  Em.runEmulatorTraceIO $ smokeTrace plutoValidator
  -- tests
