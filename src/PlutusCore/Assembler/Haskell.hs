{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

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

instance ToPluto [Char] where
  toPluto s =
    Constant () $ T () $ T.pack s

instance FromUPLC Text where
  fromUPLC = \case
    (UPLC.Constant () (PLC.Some (PLC.ValueOf PLC.DefaultUniString x))) -> pure x
    _                                                                  -> Nothing

instance FromUPLC String where
  fromUPLC = fmap T.unpack . fromUPLC
