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
import           PlutusCore.Data                     (Data)


newtype Program = Program { unProgram :: Term }
  deriving (Eq, Show)


newtype Name = Name { getName :: Text }
  deriving (Eq, Show)


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
  deriving (Eq, Show)


newtype IfTerm = IfTerm Term
  deriving (Eq, Show)


newtype ThenTerm = ThenTerm Term
  deriving (Eq, Show)


newtype ElseTerm = ElseTerm Term
  deriving (Eq, Show)


newtype LeftTerm = LeftTerm Term
  deriving (Eq, Show)


newtype RightTerm = RightTerm Term
  deriving (Eq, Show)


newtype OpTerm = OpTerm Term
  deriving (Eq, Show)


data Binding = Binding Name Term
  deriving (Eq, Show)
