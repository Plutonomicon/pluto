{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Types.InfixBuiltin (InfixBuiltin (..)) where


import           PlutusCore.Assembler.Prelude


data InfixBuiltin =
    AddInteger
  | SubtractInteger
  | MultiplyInteger
  | DivideInteger
  | RemainderInteger
  | EqualsInteger
  | LessThanInteger
  | LessThanEqualsInteger
  | AppendByteString
  | ConsByteString
  | IndexByteString
  | EqualsByteString
  | LessThanByteString
  | LessThanEqualByteString
  | AppendString
  | EqualsString
  | EqualsData
  deriving (Eq, Show, Bounded, Enum)
