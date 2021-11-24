{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Types.ErrorMessage (ErrorMessage (..)) where


import           PlutusCore.Assembler.Prelude


newtype ErrorMessage = ErrorMessage { getErrorMessage :: Text }
  deriving (Eq, Show)
