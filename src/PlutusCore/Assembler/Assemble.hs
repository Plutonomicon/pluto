{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Assemble (assemble) where


import           Codec.Serialise                         (serialise)
import           Data.ByteString.Lazy                    (toStrict)
import           Plutus.V1.Ledger.Scripts                (Script (..))

import           PlutusCore.Assembler.AnnDeBruijn        (annDeBruijn)
import           PlutusCore.Assembler.Desugar            (desugar)
import           PlutusCore.Assembler.Parse              (parse)
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Tokenize           (tokenize)
import           PlutusCore.Assembler.Types.ErrorMessage (ErrorMessage)


-- Either assemble the given code into Plutus bytecode or fail with an error message.
assemble :: Text -> Either ErrorMessage ByteString
assemble txt = toStrict . serialise . Script <$> (desugar . annDeBruijn =<< parse =<< tokenize txt)
