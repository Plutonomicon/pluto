{-# LANGUAGE TemplateHaskell #-}


module Sample.Validator.Pluto (plutoValidator) where

import           Ledger.Scripts                 (Validator (..))
import qualified PlutusCore.Assembler.Assemble  as Pluto
import qualified PlutusCore.Assembler.FFI       as PlutoFFI
import qualified PlutusCore.Assembler.Types.AST as Pluto

plutoValidatorProg :: Pluto.Program ()
plutoValidatorProg = $(PlutoFFI.load "examples/contracts/sample/validator.pluto")

plutoValidator :: Validator
plutoValidator =
  case Pluto.translate plutoValidatorProg of
    -- TODO: fail in TH instead of at (offchain) runtime
    Left err     -> Prelude.error $ Prelude.show err
    Right script -> Validator script
