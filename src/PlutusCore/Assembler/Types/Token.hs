{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Types.Token ( Token (..) ) where


import PlutusCore.Assembler.Prelude


data Builtin
data InfixBuiltin


data Token = Var Text | Lambda | Arrow | Force | Delay | OpenParen | CloseParen | Error
           | Integer Integer | ByteString ByteString | Text Text | Bool Bool
           | OpenBracket | CloseBracket | Comma | OpenBrace | CloseBrace
           | Data | Sigma | Equals | Builtin Builtin | InfixBuiltin InfixBuiltin
           | Let | Semicolon | In | If | Then | Else
