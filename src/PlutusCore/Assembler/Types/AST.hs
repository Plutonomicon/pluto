{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}


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
  , PLC.Data
  ) where


import           Data.Data                           (Data)
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Types.Builtin  (Builtin)
import           PlutusCore.Assembler.Types.Constant (Constant (..))
import qualified PlutusCore.Data                     as PLC


newtype Program ann = Program { unProgram :: Term ann }
  deriving (Eq, Show, Data, Functor, Foldable)


newtype Name = Name { getName :: Text }
  deriving (Eq, Ord, Data, Show)

instance IsString Name where
  fromString = Name . fromString


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
  deriving (Eq, Show, Data, Functor)

instance Foldable Term where
  foldMap f =
    \case
      Var a _            -> f a
      Lambda a _ t       -> f a <> foldMap f t
      Apply a g x        -> f a <> foldMap f g <> foldMap f x
      Force a x          -> f a <> foldMap f x
      Delay a x          -> f a <> foldMap f x
      Constant a x       -> f a <> foldMap f x
      Builtin a _        -> f a
      Error a            -> f a
      Let a bs x         -> f a <> foldMap (foldMap f) bs <> foldMap f x
      IfThenElse a i t e -> f a <> foldMap f i <> foldMap f t <> foldMap f e
      InfixApply a l o r -> f a <> foldMap f l <> foldMap f o <> foldMap f r


newtype IfTerm ann = IfTerm (Term ann)
  deriving (Eq, Show, Data, Functor, Foldable)


newtype ThenTerm ann = ThenTerm (Term ann)
  deriving (Eq, Show, Data, Functor, Foldable)


newtype ElseTerm ann = ElseTerm (Term ann)
  deriving (Eq, Show, Data, Functor, Foldable)


newtype LeftTerm ann = LeftTerm (Term ann)
  deriving (Eq, Show, Data, Functor, Foldable)


newtype RightTerm ann = RightTerm (Term ann)
  deriving (Eq, Show, Data, Functor, Foldable)


newtype OpTerm ann = OpTerm (Term ann)
  deriving (Eq, Show, Data, Functor, Foldable)


data Binding ann = Binding ann Name (Term ann)
  deriving (Eq, Show, Data, Functor)

instance Foldable Binding where
  foldMap f (Binding a _ x) =
    f a <> foldMap f x
