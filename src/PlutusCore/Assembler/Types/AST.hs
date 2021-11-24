{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Types.AST
  ( Program (..)
  , Name (..)
  , Term (..)
  , IfTerm (..)
  , ThenTerm (..)
  , ElseTerm (..)
  , LeftTerm (..)
  , RightTerm (..)
  , OpTerm (..)
  , Binding (..)
  , Constant (..)
  , Builtin
  , Data
  ) where


import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Types.Builtin  (Builtin)
import           PlutusCore.Assembler.Types.Constant (Constant (..))
import PlutusCore.Data (Data)


newtype Program = Program { unProgram :: Term }


newtype Name = Name { getName :: Text }


data Term =
    Var Name
  | Lambda Binding
  | Apply Term Term
  | Force Term
  | Delay Term
  | Constant Constant
  | Builtin Builtin
  | Error
  | Let [Binding] Term
  | IfThenElse IfTerm ThenTerm ElseTerm
  | InfixApply LeftTerm OpTerm RightTerm


newtype IfTerm = IfTerm Term


newtype ThenTerm = ThenTerm Term


newtype ElseTerm = ElseTerm Term


newtype LeftTerm = LeftTerm Term


newtype RightTerm = RightTerm Term


newtype OpTerm = OpTerm Term


data Binding = Binding Name Term
