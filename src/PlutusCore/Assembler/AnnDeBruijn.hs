{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}


module PlutusCore.Assembler.AnnDeBruijn
  ( annDeBruijn
  , addNameToMap
  ) where


import qualified Data.Map                       as Map

import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Types.AST (Binding, Name, Program, Term)
import qualified PlutusCore.Assembler.Types.AST as AST
import           PlutusCore.DeBruijn            (DeBruijn (..), Index (..))


-- Amend the annotations with a lookup table of de Bruijn indices
-- for each subterm of the program.
annDeBruijn :: Program a -> Program (a, Map Name DeBruijn)
annDeBruijn (AST.Program t) = AST.Program (annTerm mempty t)


annTerm :: Map Name DeBruijn -> Term a -> Term (a, Map Name DeBruijn)
annTerm m =
  \case
    AST.Var a x -> AST.Var (a, m) x
    AST.Lambda a xs y ->
      let m' = foldl addNameToMap m xs
      in AST.Lambda (a, m') xs (annTerm m' y)
    AST.Apply a f x -> AST.Apply (a, m) (annTerm m f) (annTerm m x)
    AST.Force a x -> AST.Force (a, m) (annTerm m x)
    AST.Delay a x -> AST.Delay (a, m) (annTerm m x)
    AST.Constant a x -> AST.Constant (a, m) ((,m) <$> x)
    AST.Builtin a x -> AST.Builtin (a, m) x
    AST.Error a -> AST.Error (a, m)
    AST.Let a bs x ->
      let (bs', m') = annBindings m (letReverse bs)
      in AST.Let (a, m) (letReverse bs') (annTerm m' x)
    AST.IfThenElse a (AST.IfTerm i) (AST.ThenTerm t) (AST.ElseTerm e) ->
      AST.IfThenElse (a, m)
        (AST.IfTerm (annTerm m i))
        (AST.ThenTerm (annTerm m t))
        (AST.ElseTerm (annTerm m e))
    AST.InfixApply a (AST.LeftTerm l) (AST.OpTerm o) (AST.RightTerm r) ->
      AST.InfixApply (a, m)
        (AST.LeftTerm (annTerm m l))
        (AST.OpTerm (annTerm m o))
        (AST.RightTerm (annTerm m r))


annBindings :: Map Name DeBruijn
            -> [Binding a]
            -> ( [Binding (a, Map Name DeBruijn)]
               , Map Name DeBruijn
               )
annBindings m [] = ( [], m )
annBindings m ( AST.Binding a x t : bs ) =
  let m' = addNameToMap m x
      (bs', m'') = annBindings m' bs
  in ( AST.Binding (a, m) x (annTerm m' t)
       : bs'
     , m''
     )


addNameToMap :: Map Name DeBruijn -> Name -> Map Name DeBruijn
addNameToMap m n =
  Map.insert n (DeBruijn firstIndex) (inc <$> m)
  where
    inc (DeBruijn (Index i)) = DeBruijn (Index (i+1))
    firstIndex = Index 1
