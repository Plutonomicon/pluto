{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Types.Constant
  ( Constant (..)
  ) where


import           PlutusCore.Assembler.Prelude
import           PlutusCore.Data              (Data)


data Constant =
    I Integer
  | S ByteString
  | T Text
  | U
  | B Bool
  | L [Constant]
  | P (Constant, Constant)
  | D Data
