{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module PlutusCore.Assembler.Haskell
  ( -- * Converting to/from Haskell types
    ToPluto(..),
    FromUPLC(..),
  )
where

import qualified Data.Text                      as T
import qualified PlutusCore                     as PLC
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Types.AST
import qualified PlutusTx.IsData.Class          as Plutus
import qualified UntypedPlutusCore              as UPLC

-- | Class of all Haskell values that can be represented in Pluto
class ToPluto a where
  toPluto :: a -> Term ()

-- | Class of all Haskell values that are represented in UPLC
class FromUPLC a where
  fromUPLC :: UPLC.Term name PLC.DefaultUni fun () -> Maybe a

instance ToPluto Text where
  toPluto s =
    Constant () $ T () s

-- Overlaps the ToData  instance below, because strings are represented
-- internally, as opposed to being opaque in a `Data`.
instance {-# OVERLAPPING #-} ToPluto [Char] where
  toPluto s =
    Constant () $ T () $ T.pack s

instance ToPluto Integer where
  toPluto x =
    Constant () $ I () x

instance {-# OVERLAPPABLE #-} Plutus.ToData a => ToPluto a where
  toPluto xs =
    Constant () $ D () $ Plutus.toData xs

instance FromUPLC Integer where
  fromUPLC = \case
    (UPLC.Constant () (PLC.Some (PLC.ValueOf PLC.DefaultUniInteger x))) -> pure x
    _                                                                  -> Nothing
instance FromUPLC Text where
  fromUPLC = \case
    (UPLC.Constant () (PLC.Some (PLC.ValueOf PLC.DefaultUniString x))) -> pure x
    _                                                                  -> Nothing

instance FromUPLC String where
  fromUPLC = fmap T.unpack . fromUPLC

instance FromUPLC Data where
  fromUPLC = \case
    (UPLC.Constant () (PLC.Some (PLC.ValueOf PLC.DefaultUniData x))) -> pure x
    _                                                                -> Nothing

