{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Assemble (assemble, translate, parseProgram) where


import           Codec.Serialise                         (serialise)
import           Data.ByteString.Lazy                    (toStrict)
import           Plutus.V1.Ledger.Scripts                (Script (..))

import           PlutusCore.Assembler.AnnDeBruijn        (annDeBruijn)
import           PlutusCore.Assembler.Desugar            (desugar)
import           PlutusCore.Assembler.Parse              (parse)
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Tokenize           (tokenize)
import           PlutusCore.Assembler.Types.AST          (Program)
import           PlutusCore.Assembler.Types.ErrorMessage (ErrorMessage)
import           Text.Parsec.Pos                         (SourcePos)


-- | Either assemble the given code into Plutus bytecode or fail with an error message.
assemble :: Text -> Either ErrorMessage ByteString
assemble = fmap (toStrict . serialise) . translate <=< parseProgram


-- | Translatre the given Pluto code into a Plutus Script
translate :: Program SourcePos -> Either ErrorMessage Script
translate = fmap Script . (desugar . annDeBruijn)

parseProgram :: Text -> Either ErrorMessage (Program SourcePos)
parseProgram =
  parse <=< tokenize
