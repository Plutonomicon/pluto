{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Assemble (assemble, translate) where


import           Codec.Serialise                         (serialise)
import           Data.ByteString.Lazy                    (toStrict)
import           Plutus.V1.Ledger.Scripts                (Script (..))

import           PlutusCore.Assembler.AnnDeBruijn        (annDeBruijn)
import           PlutusCore.Assembler.Desugar            (desugar)
import           PlutusCore.Assembler.Parse              (parse)
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Tokenize           (tokenize)
import           PlutusCore.Assembler.Types.ErrorMessage (ErrorMessage)


-- | Either assemble the given code into Plutus bytecode or fail with an error message.
assemble :: Text -> Either ErrorMessage ByteString
assemble = fmap (toStrict . serialise) . translate


-- | Translatre the given Pluto code into a Plutus Script
translate :: Text -> Either ErrorMessage Script
translate txt = Script <$> (desugar . annDeBruijn =<< parse =<< tokenize txt)
