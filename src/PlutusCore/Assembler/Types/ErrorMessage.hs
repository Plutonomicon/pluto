{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Types.ErrorMessage (ErrorMessage (..)) where


import           Control.Exception            (Exception)
import           PlutusCore.Assembler.Prelude


newtype ErrorMessage = ErrorMessage { getErrorMessage :: Text }
  deriving (Eq, Show, Exception)
