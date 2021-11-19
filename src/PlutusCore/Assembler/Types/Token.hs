{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Types.Token ( Token (..) ) where


import Data.Text (Text)


data Token = Var Text | Force | Delay
