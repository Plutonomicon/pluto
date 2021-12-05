{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module PlutusCore.Assembler.Desugar ( desugar ) where


import qualified Data.Map                                as Map
import qualified Data.Text                               as Text
import qualified PlutusCore.Core                         as PLC
import           PlutusCore.DeBruijn                     (DeBruijn (..),
                                                          Index (..))
import           PlutusCore.Default                      (DefaultFun,
                                                          DefaultUni, Some,
                                                          ValueOf)
import qualified PlutusCore.Default                      as PLC
import qualified UntypedPlutusCore.Core.Type             as UPLC

import           PlutusCore.Assembler.AnnDeBruijn        (addNameToMap,
                                                          incDeBruijn)
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Types.AST          (Binding, Builtin,
                                                          Constant, Name,
                                                          Program, Term)
import qualified PlutusCore.Assembler.Types.AST          as AST
import           PlutusCore.Assembler.Types.Builtin      (Builtin (..))
import           PlutusCore.Assembler.Types.ErrorMessage (ErrorMessage (..))


type UnsweetProgram = UPLC.Program DeBruijn DefaultUni DefaultFun ()
type UnsweetTerm = UPLC.Term DeBruijn DefaultUni DefaultFun ()


desugar :: Show ann
        => Program (ann, Map Name DeBruijn)
        -> Either ErrorMessage UnsweetProgram
desugar (AST.Program x) =
  UPLC.Program () (PLC.defaultVersion ()) <$> desugarTerm x


desugarTerm :: Show ann
            => Term (ann, Map Name DeBruijn)
            -> Either ErrorMessage UnsweetTerm
desugarTerm =
  \case
    AST.Var (p, m) x ->
      case Map.lookup x m of
        Just i -> pure (UPLC.Var () i)
        Nothing -> Left (ErrorMessage $ "undefined variable name '" <> AST.getName x <> "' at source position " <> Text.pack (show p))
    AST.Lambda (p, _) [] _ -> Left (ErrorMessage $ "lambda with no names at source position " <> Text.pack (show p))
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
    AST.Let _ bs x -> desugarLet bs x
    AST.IfThenElse _ (AST.IfTerm i) (AST.ThenTerm t) (AST.ElseTerm e) ->
      evalLazy
      <$> ((lazify2 $ UPLC.Apply ())
           <$> (lazify
                <$> (UPLC.Apply ()
                     <$> (UPLC.Apply ()
                            (UPLC.Force () (UPLC.Builtin () PLC.IfThenElse))
                          <$> desugarTerm i
                         )
                    )
                 <*> lazy t
               )
           <*> lazy e
          )
    AST.InfixApply _ (AST.LeftTerm l) (AST.OpTerm o) (AST.RightTerm r) ->
      UPLC.Apply ()
        <$> ( UPLC.Apply ()
                <$> desugarTerm o
                <*> desugarTerm l )
        <*> desugarTerm r



newtype Lazy = Lazy UnsweetTerm

lazy :: Show a => Term (a, Map Name DeBruijn) -> Either ErrorMessage Lazy
lazy t =
  Lazy . UPLC.Delay () <$> desugarTerm (inc t)
  where
    inc = fmap (\(a, m) -> (a, incDeBruijn <$> m))

evalLazy :: Lazy -> UnsweetTerm
evalLazy (Lazy t) = UPLC.Force () t

lazify :: (UnsweetTerm -> UnsweetTerm) -> Lazy -> Lazy
lazify f (Lazy t) = Lazy $ f t

lazify2 :: (UnsweetTerm -> UnsweetTerm -> UnsweetTerm) -> Lazy -> Lazy -> Lazy
lazify2 f (Lazy t) (Lazy u) = Lazy $ f t u

-- We pass in the bindings innermost first instead of the usual outermost
-- first convention in order to simplify the recursion.
desugarLet :: Show ann => [Binding (ann, Map Name DeBruijn)] -> Term (ann, Map Name DeBruijn) -> Either ErrorMessage UnsweetTerm
desugarLet [] y = desugarTerm y -- allow this case to make the recursion simpler
desugarLet ( AST.Binding _ _ e : bs ) y =
  UPLC.Apply ()
  <$> ( UPLC.LamAbs () (DeBruijn (Index 0)) -- TODO: is this right?
        <$> desugarLet bs y )
  <*> desugarTerm e


desugarConstant :: Constant ann -> Some (ValueOf DefaultUni)
desugarConstant =
  \case
    AST.I _ x -> PLC.Some (PLC.ValueOf PLC.DefaultUniInteger x)
    AST.S _ x -> PLC.Some (PLC.ValueOf PLC.DefaultUniByteString x)
    AST.T _ x -> PLC.Some (PLC.ValueOf PLC.DefaultUniString x)
    AST.U _   -> PLC.Some (PLC.ValueOf PLC.DefaultUniUnit ())
    AST.B _ x -> PLC.Some (PLC.ValueOf PLC.DefaultUniBool x)
    AST.D _ x -> PLC.Some (PLC.ValueOf PLC.DefaultUniData x)


desugarBuiltin :: Builtin -> DefaultFun
desugarBuiltin =
  \case
    AddInteger              -> PLC.AddInteger
    SubtractInteger         -> PLC.SubtractInteger
    MultiplyInteger         -> PLC.MultiplyInteger
    DivideInteger           -> PLC.DivideInteger
    QuotientInteger         -> PLC.QuotientInteger
    RemainderInteger        -> PLC.RemainderInteger
    ModInteger              -> PLC.ModInteger
    EqualsInteger           -> PLC.EqualsInteger
    LessThanInteger         -> PLC.LessThanInteger
    LessThanEqualsInteger   -> PLC.LessThanEqualsInteger
    AppendByteString        -> PLC.AppendByteString
    ConsByteString          -> PLC.ConsByteString
    SliceByteString         -> PLC.SliceByteString
    LengthByteString        -> PLC.LengthOfByteString
    IndexByteString         -> PLC.IndexByteString
    EqualsByteString        -> PLC.EqualsByteString
    LessThanByteString      -> PLC.LessThanByteString
    LessThanEqualByteString -> PLC.LessThanEqualsByteString
    Sha2_256                -> PLC.Sha2_256
    Sha3_256                -> PLC.Sha3_256
    Blake2b_256             -> PLC.Blake2b_256
    VerifySignature         -> PLC.VerifySignature
    AppendString            -> PLC.AppendString
    EqualsString            -> PLC.EqualsString
    EncodeUtf8              -> PLC.EncodeUtf8
    DecodeUtf8              -> PLC.DecodeUtf8
    IfThenElse              -> PLC.IfThenElse
    ChooseUnit              -> PLC.ChooseUnit
    Trace                   -> PLC.Trace
    FstPair                 -> PLC.FstPair
    SndPair                 -> PLC.SndPair
    ChooseList              -> PLC.ChooseList
    MkCons                  -> PLC.MkCons
    HeadList                -> PLC.HeadList
    TailList                -> PLC.TailList
    NullList                -> PLC.NullList
    ChooseData              -> PLC.ChooseData
    ConstrData              -> PLC.ConstrData
    MapData                 -> PLC.MapData
    ListData                -> PLC.ListData
    IData                   -> PLC.IData
    BData                   -> PLC.BData
    UnConstrData            -> PLC.UnConstrData
    UnMapData               -> PLC.UnMapData
    UnBData                 -> PLC.UnBData
    EqualsData              -> PLC.EqualsData
    MkPairData              -> PLC.MkPairData
    MkNilData               -> PLC.MkNilData
    MkNilPairData           -> PLC.MkNilPairData
