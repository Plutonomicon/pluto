{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Assemble (assemble, assembleAndShrink, translate, parseProgram, parsePlutusData) where


import           Codec.Serialise                         (serialise)
import           Data.ByteString.Lazy                    (toStrict)
import           Plutus.V1.Ledger.Scripts                (Script (..))

import           PlutusCore.Assembler.AnnDeBruijn        (annDeBruijn)
import           PlutusCore.Assembler.Desugar            (desugar)
import           PlutusCore.Assembler.Parse              (parse, parseData)
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Tokenize           (tokenize)
import           PlutusCore.Assembler.Types.AST          (Program)
import           PlutusCore.Assembler.Types.ErrorMessage (ErrorMessage)
import qualified PlutusCore.Data                         as PLC
import           Text.Parsec.Pos                         (SourceName, SourcePos)
import           Shrink                                  (shrinkScript)


-- | Either assemble the given code into Plutus bytecode or fail with an error message.
assemble :: SourceName -> Text -> Either ErrorMessage ByteString
assemble name = fmap (toStrict . serialise) . translate <=< parseProgram name

assembleAndShrink :: SourceName -> Text -> Either ErrorMessage ByteString
assembleAndShrink name = fmap (toStrict . serialise . shrinkScript) . translate <=< parseProgram name

-- | Translatre the given Pluto code into a Plutus Script
translate :: Show ann => Program ann -> Either ErrorMessage Script
translate = fmap Script . (desugar . annDeBruijn)


parseProgram :: SourceName -> Text -> Either ErrorMessage (Program SourcePos)
parseProgram name =
  parse name <=< tokenize name

parsePlutusData :: SourceName -> Text -> Either ErrorMessage PLC.Data
parsePlutusData name =
  parseData name <=< tokenize name
