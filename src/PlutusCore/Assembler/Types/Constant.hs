{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module PlutusCore.Assembler.Types.Constant
  ( Constant (..)
  ) where


import           Data.Data                    (Data)
import           PlutusCore.Assembler.Prelude
import qualified PlutusCore.Data              as PLC

-- TODO: Should we upstream this? Might that affect onchain code size?
deriving instance Data PLC.Data

data Constant ann =
    I ann Integer
  | S ann ByteString
  | T ann Text
  | U ann
  | B ann Bool
  | D ann PLC.Data
  deriving (Eq, Show, Data, Functor)

instance Foldable Constant where
  foldMap f =
    \case
      I a _ -> f a
      S a _ -> f a
      T a _ -> f a
      U a   -> f a
      B a _ -> f a
      D a _ -> f a
