{-# LANGUAGE TemplateHaskell #-}

module PlutusCore.Assembler.ShrinkTH
  (shrinkCompiledTH) where

import Language.Haskell.TH
import PlutusTx.Code (CompiledCode)
import PlutusCore.Assembler.Shrink(shrinkCompiled)

shrinkCompiledTH :: Q (TExp (CompiledCode a)) -> Q (TExp (CompiledCode a))
shrinkCompiledTH q = [|| shrinkCompiled $$( q ) ||] 
