{-# LANGUAGE DeriveFunctor     #-}
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


newtype Program ann = Program { unProgram :: Term ann }
  deriving (Eq, Show, Functor)


newtype Name = Name { getName :: Text }
  deriving (Eq, Ord, Show)


data Term ann =
    Var ann Name
  | Lambda ann [Name] (Term ann)
  | Apply ann (Term ann) (Term ann)
  | Force ann (Term ann)
  | Delay ann (Term ann)
  | Constant ann (Constant ann)
  | Builtin ann Builtin
  | Error ann
  | Let ann [Binding ann] (Term ann)
  | IfThenElse ann (IfTerm ann) (ThenTerm ann) (ElseTerm ann)
  | InfixApply ann (LeftTerm ann) (OpTerm ann) (RightTerm ann)
  deriving (Eq, Show, Functor)


newtype IfTerm ann = IfTerm (Term ann)
  deriving (Eq, Show, Functor)


newtype ThenTerm ann = ThenTerm (Term ann)
  deriving (Eq, Show, Functor)


newtype ElseTerm ann = ElseTerm (Term ann)
  deriving (Eq, Show, Functor)


newtype LeftTerm ann = LeftTerm (Term ann)
  deriving (Eq, Show, Functor)


newtype RightTerm ann = RightTerm (Term ann)
  deriving (Eq, Show, Functor)


newtype OpTerm ann = OpTerm (Term ann)
  deriving (Eq, Show, Functor)


data Binding ann = Binding ann Name (Term ann)
  deriving (Eq, Show, Functor)
