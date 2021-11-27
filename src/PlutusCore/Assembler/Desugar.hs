{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module PlutusCore.Assembler.Desugar ( desugar ) where


import Text.Parsec.Pos (SourcePos)
import PlutusCore.DeBruijn (DeBruijn)
import PlutusCore.Default (DefaultUni, DefaultFun)
import qualified UntypedPlutusCore.Core.Type as UPLC
import qualified PlutusCore.Core as PLC
import qualified Data.Map as Map
import qualified Data.Text as Text

import PlutusCore.Assembler.Prelude
import PlutusCore.Assembler.Types.AST (Program, Term, Name)
import qualified PlutusCore.Assembler.Types.AST as AST


type UnsweetProgram = UPLC.Program DeBruijn DefaultUni DefaultFun ()
type UnsweetTerm = UPLC.Term DeBruijn DefaultUni DefaultFun ()


desugar :: Program (SourcePos, Map Name DeBruijn)
        -> Either Text UnsweetProgram
desugar (AST.Program x) =
  UPLC.Program () (PLC.defaultVersion ()) <$> desugarTerm x


desugarTerm :: Term (SourcePos, Map Name DeBruijn)
            -> Either Text UnsweetTerm
desugarTerm =
  \case
    AST.Var (p, m) x ->
      case Map.lookup x m of
        Just i -> pure (UPLC.Var () i)
        Nothing -> Left ("undefined variable name " <> AST.getName x <> " at source position " <> Text.pack (show p))
    _ -> todo


todo :: a
todo = todo
