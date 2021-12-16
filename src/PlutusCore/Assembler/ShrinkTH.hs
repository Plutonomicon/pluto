{-# LANGUAGE TemplateHaskell #-}

module PlutusCore.Assembler.ShrinkTH
  (shrinkCompiledTH) where

import           Language.Haskell.TH
import           PlutusCore.Assembler.Shrink (shrinkCompiled)
import           PlutusTx.Code               (CompiledCode)

shrinkCompiledTH :: Q (TExp (CompiledCode a)) -> Q (TExp (CompiledCode a))
shrinkCompiledTH q = [|| shrinkCompiled $$( q ) ||]
