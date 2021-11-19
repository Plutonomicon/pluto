{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Tokenize
  ( tokenize
  , Token (..)
  , ErrorMessage (..)
  ) where


import PlutusCore.Assembler.Prelude
import PlutusCore.Assembler.Types.Token (Token (..))


newtype ErrorMessage = ErrorMessage { getErrorMessage :: Text }


tokenize :: Text -> Either ErrorMessage [Token]
tokenize = todo


todo :: a
todo = todo
