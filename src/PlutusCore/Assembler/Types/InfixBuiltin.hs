{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Types.InfixBuiltin
  ( InfixBuiltin (..)
  , toBuiltin
  ) where


import           PlutusCore.Assembler.Prelude
import qualified PlutusCore.Assembler.Types.Builtin as B


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


toBuiltin :: InfixBuiltin -> B.Builtin
toBuiltin =
  \case
    AddInteger -> B.AddInteger
    SubtractInteger -> B.SubtractInteger
    MultiplyInteger -> B.MultiplyInteger
    DivideInteger -> B.DivideInteger
    RemainderInteger -> B.RemainderInteger
    EqualsInteger -> B.EqualsInteger
    LessThanInteger -> B.LessThanInteger
    LessThanEqualsInteger -> B.LessThanEqualsInteger
    AppendByteString -> B.AppendByteString
    ConsByteString -> B.ConsByteString
    IndexByteString -> B.IndexByteString
    EqualsByteString -> B.EqualsByteString
    LessThanByteString -> B.LessThanByteString
    LessThanEqualByteString -> B.LessThanEqualByteString
    AppendString -> B.AppendString
    EqualsString -> B.EqualsString
    EqualsData -> B.EqualsData
