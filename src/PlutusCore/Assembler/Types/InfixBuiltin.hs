{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Types.InfixBuiltin (InfixBuiltin (..)) where


data InfixBuiltin =
    AddInteger 
  | SubtractInteger
  | MultiplyInteger
  | DivideInteger
  | RemainderInteger
  | ModInteger
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
