{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module PlutusCore.Assembler.Desugar ( desugar ) where


import Text.Parsec.Pos (SourcePos)
import PlutusCore.DeBruijn (DeBruijn (..), Index (..))
import PlutusCore.Default (Some, ValueOf, DefaultUni, DefaultFun)
import qualified UntypedPlutusCore.Core.Type as UPLC
import qualified PlutusCore.Core as PLC
import qualified Data.Map as Map
import qualified Data.Text as Text

import PlutusCore.Assembler.Prelude
import PlutusCore.Assembler.Types.AST (Program, Term, Name, Constant, Builtin)
import qualified PlutusCore.Assembler.Types.AST as AST
import PlutusCore.Assembler.AnnDeBruijn (addNameToMap)


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
    AST.Lambda (p, _) [] _ -> Left ("lambda with no names at source position " <> Text.pack (show p))
    AST.Lambda _ [_] y ->
      UPLC.LamAbs () (DeBruijn (Index 0)) -- TODO: is this right?
        <$> desugarTerm y
    AST.Lambda (p, m) (x:xs) y ->
      UPLC.LamAbs () (DeBruijn (Index 0)) -- TODO: is this right?
        <$> desugarTerm (AST.Lambda (p, addNameToMap m x) xs y)
    AST.Apply _ f x ->
      UPLC.Apply () <$> desugarTerm f <*> desugarTerm x
    AST.Force _ x -> UPLC.Force () <$> desugarTerm x
    AST.Delay _ x -> UPLC.Delay () <$> desugarTerm x
    AST.Constant _ x -> pure (UPLC.Constant () (desugarConstant x))
    AST.Builtin _ f -> pure (UPLC.Builtin () (desugarBuiltin f))
    AST.Error _ -> pure (UPLC.Error ())
    _ -> todo


desugarConstant :: Constant ann -> Some (ValueOf DefaultUni)
desugarConstant = todo


desugarBuiltin :: Builtin -> DefaultFun
desugarBuiltin = todo


todo :: a
todo = todo
