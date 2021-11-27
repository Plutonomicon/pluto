{-# LANGUAGE DeriveFunctor     #-}
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
