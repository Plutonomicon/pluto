{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Types.Constant
  ( Constant (..)
  ) where


import           PlutusCore.Assembler.Prelude
import           PlutusCore.Data              (Data)


data Constant ann =
    I ann Integer
  | S ann ByteString
  | T ann Text
  | U ann
  | B ann Bool
  | L ann [Constant ann]
  | P ann (Constant ann, Constant ann)
  | D ann Data
  deriving (Eq, Show, Functor)

instance Foldable Constant where
  foldMap f =
    \case
      I a _ -> f a
      S a _ -> f a
      T a _ -> f a
      U a   -> f a
      B a _ -> f a
      L a cs -> f a <> foldMap (foldMap f) cs
      P a (x, y) -> f a <> foldMap f x <> foldMap f y
      D a _ -> f a
