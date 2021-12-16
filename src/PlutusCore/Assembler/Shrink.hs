{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module PlutusCore.Assembler.Shrink
  (shrinkCompiled
  ,shrinkCompiledSp
  ,withoutTactics
  ,shrinkScript
  ,shrinkProgram
  -- most of these exports are intended for testing
  ,Program
  ,Tactic
  ,SafeTactic
  ,DTerm
  ,NTerm
  ,dTermToN
  ,nTermToD
  ,defaultShrinkParams
  ,tactics
  ,safeTactics
  ,size
  ,stepShrink
              )where


import           Codec.Serialise
import           Data.ByteString.Lazy         (toStrict)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Functor.Identity (runIdentity)
import           Data.List                    (sortOn,filter,notElem,group,groupBy,maximum)
import           Data.Set                     (Set)
import qualified Data.Set                     as S
import           Data.Text                    (pack)
import Data.Maybe (fromMaybe)
import           Control.Monad                (sequence,replicateM)
import Control.Monad.State                   (StateT,State,evalStateT,get,modify)
import           Control.Monad.Reader         (ReaderT,MonadReader,runReaderT,local)
import           Plutus.V1.Ledger.Scripts     (Script (..), fromCompiledCode,
                                               scriptSize)
import           PlutusCore                   (FreeVariableError,runQuoteT)
import           PlutusCore.Assembler.Prelude
import           PlutusCore.DeBruijn          (DeBruijn (..), 
                                               fakeNameDeBruijn)
import           PlutusCore.Default           (DefaultFun (..), DefaultUni)
import           PlutusTx.Code                (CompiledCode,
                                               CompiledCodeIn (..))
import           UntypedPlutusCore            (Name(..),Unique(..),Version(..),termMapNames,programMapNames,unDeBruijnTerm,deBruijnTerm,unNameDeBruijn)
import           UntypedPlutusCore.Core.Type  (Term(..),Program(..))

type DTerm    = Term    DeBruijn DefaultUni DefaultFun ()
type NTerm    = Term    Name     DefaultUni DefaultFun ()
type DProgram = Program DeBruijn DefaultUni DefaultFun ()

type Scope = Set Name
type ScopeMT m a = ReaderT (Scope,Scope) (StateT Integer m) a
type ScopeM    a = ReaderT (Scope,Scope) (State  Integer  ) a

type SafeTactic = NTerm -> NTerm
-- safe tactics are shortening strategies which
-- can never be counter productive
type Tactic = NTerm -> [NTerm]
-- tactics are ways of shortening programs
-- because they can be counter productive
-- they return a list of terms gotten by
-- applying the tactic at different points
-- in the program. The head of the list is
-- reservered for the original term
type PartialTactic = NTerm -> ScopeM (Maybe [NTerm])
type ScopedTactic  = NTerm -> ScopeM [NTerm]
--type ScopedSafe    = NTerm -> ScopeM NTerm

data ShrinkParams = ShrinkParams
  { safeTactics     :: [(String,SafeTactic)]
  , tactics         :: [(String,Tactic)]
  , parallelTactics :: Integer
  , parallelTerms   :: Integer
  , extraSteps      :: Integer
  }
-- Tactics are stored with strings so the tests can
-- automatically add the name to the name of the
-- property test

data WhnfRes = Err | Unclear  | Safe deriving (Eq,Ord)

runScopeMT :: Monad m => NTerm -> ScopeMT m a -> m a
runScopeMT nt smtma = let
  globalScope = usedScope nt
  free = fromIntegral$1+maximum (0:(unUnique . nameUnique <$> S.toList globalScope))
    in evalStateT (runReaderT smtma (globalScope,S.empty)) free

runScopeM :: NTerm -> ScopeM a -> a
runScopeM nt = runIdentity . runScopeMT nt

usedScope :: NTerm -> Scope
usedScope = \case
  Var _ n -> S.singleton n
  LamAbs _ n t -> S.insert n (usedScope t)
  Force _ t -> usedScope t
  Delay _ t -> usedScope t
  Apply _ f x -> S.union (usedScope f) (usedScope x)
  _ -> S.empty

runScopedTact :: (NTerm -> ScopeM a) -> NTerm -> a
runScopedTact f nt = runScopeM nt (f nt)

withoutTactics :: [String] -> ShrinkParams
withoutTactics ts = defaultShrinkParams
  { safeTactics = filter (\(tn,_) -> tn `notElem` ts) (safeTactics defaultShrinkParams)
  , tactics     = filter (\(tn,_) -> tn `notElem` ts) (tactics     defaultShrinkParams)
  }

shrinkCompiled :: CompiledCode a -> CompiledCode a
shrinkCompiled = shrinkCompiledSp defaultShrinkParams

shrinkCompiledSp :: ShrinkParams -> CompiledCode a -> CompiledCode a
shrinkCompiledSp sp comped = let
  asScript = fromCompiledCode comped
  script@(Script prog') = shrinkScriptSp sp asScript
  prog = programMapNames fakeNameDeBruijn prog'
  scriptBc = toStrict $ serialise script
    in case comped of
         SerializedCode   _ maybePirByteString a -> SerializedCode   scriptBc maybePirByteString  a
         DeserializedCode _ maybePir           a -> DeserializedCode prog     maybePir            a

shrinkScript :: Script -> Script
shrinkScript = shrinkScriptSp defaultShrinkParams

shrinkScriptSp :: ShrinkParams -> Script -> Script
shrinkScriptSp sp (Script prog) = Script (shrinkProgramSp sp prog)

shrinkProgram :: DProgram -> DProgram
shrinkProgram = shrinkProgramSp defaultShrinkParams

shrinkProgramSp :: ShrinkParams -> DProgram -> DProgram
shrinkProgramSp sp (Program _ version term) = Program () version (shrinkDTermSp sp term)

nTermToD :: NTerm -> DTerm
nTermToD = termMapNames unNameDeBruijn . (\case 
  Right t -> t
  Left s -> error $ "nTermToD failed with" ++ show (s :: FreeVariableError)
  ) . runQuoteT . deBruijnTerm 

dTermToN :: DTerm -> NTerm
dTermToN = (\case 
  Right t -> t
  Left s -> error $ "dTermToN failed with" ++ show (s :: FreeVariableError)
  ) . runQuoteT . unDeBruijnTerm . termMapNames fakeNameDeBruijn

--shrinkDTerm :: DTerm -> DTerm
--shrinkDTerm = shrinkDTermSp defaultShrinkParams

shrinkDTermSp :: ShrinkParams -> DTerm -> DTerm
shrinkDTermSp sp = nTermToD . shrinkNTermSp sp . dTermToN

--shrinkNTerm :: NTerm -> NTerm
--shrinkNTerm = shrinkNTermSp defaultShrinkParams

shrinkNTermSp :: ShrinkParams -> NTerm -> NTerm
shrinkNTermSp sp = runShrink (extraSteps sp) sp . return

runShrink :: Integer -> ShrinkParams -> [NTerm] -> NTerm
runShrink es sp !terms
  | size (head terms) > size (head terms') = runShrink (extraSteps sp) sp terms'
  | es > 0                                 = runShrink (es -1)         sp terms'
  | otherwise                              = head terms
    where
      terms' = stepShrink sp terms

stepShrink :: ShrinkParams -> [NTerm] -> [NTerm]
stepShrink sp terms = let
  terms' = fmap (foldl (.) id (snd <$> safeTactics sp)) terms
  cands = do
    tacts <- replicateM (fromIntegral $ parallelTactics sp) (snd <$> tactics sp)
    foldl (>>=) terms' tacts
  sizedCands = [(size c,c) | c <- cands ]
  batches = groupBy ((==) `on` fst) . sortOn fst $ sizedCands
  uniques = concat $ fmap head . group . sortOn (show . snd) <$> batches
  in take (fromIntegral $ parallelTerms sp) (snd <$> uniques)

size :: NTerm -> Integer
size = sizeD . nTermToD

sizeD :: DTerm -> Integer
sizeD = scriptSize . Script . Program () (Version () 0 0 0)

defaultShrinkParams :: ShrinkParams
defaultShrinkParams = ShrinkParams
  { safeTactics = [("removeDeadCode",removeDeadCode),("clean pairs",cleanPairs)]
  , tactics = [("subs",subs),("unsubs",unsubs),("curry",uplcCurry)]
  , parallelTactics = 1
  , parallelTerms = 20
  , extraSteps = 5
  }

-- Utilities to make tactics simpler

completeTactic :: PartialTactic -> Tactic
completeTactic = runScopedTact . completeTactic'

completeTactic' :: PartialTactic -> ScopedTactic
completeTactic' pt term = do
  let st = completeTactic' pt
  extras <- fromMaybe [] <$> pt term
  descend st term <&> (++ extras)

descend :: ScopedTactic -> ScopedTactic
descend tact = \case
       Var _ name -> return [Var () name]
       LamAbs _ name term -> fmap (LamAbs () name) <$> addNameToScope name (tact term)
       Apply _ funTerm varTerm -> do
         funTerms <- tact funTerm
         varTerms <- tact varTerm
         return $ Apply () funTerm varTerm :
               [Apply () funTerm' varTerm  | funTerm' <- drop 1 funTerms ]
            ++ [Apply () funTerm  varTerm' | varTerm' <- drop 1 varTerms ]
       Force _ term -> fmap (Force ()) <$> tact term
       Delay _ term -> fmap (Delay ()) <$> tact term
       Constant _ val -> return [Constant () val]
       Builtin _ fun  -> return [Builtin () fun]
       Error _ -> return [Error ()]

addNameToScope :: MonadReader (Scope,Scope) m => Name -> m a -> m a
addNameToScope name = local $ second (S.insert name)

completeRec :: (NTerm -> Maybe NTerm) -> NTerm -> NTerm
completeRec partial originalTerm = let
  rec = completeRec partial
    in case partial originalTerm of
      Just term -> term
      Nothing ->
        case originalTerm of
          LamAbs _ name term -> LamAbs () name (rec term)
          Apply  _ f x       -> Apply  () (rec f) (rec x)
          Force  _ term      -> Force  () (rec term)
          Delay  _ term      -> Delay  () (rec term)
          term               -> term

appBind :: Name -> NTerm -> NTerm -> NTerm
appBind name val = completeRec $ \case
      Var _ varName -> if name == varName
                               then Just val
                               else Nothing
      _ -> Nothing

mentions :: Name -> NTerm -> Bool
mentions name = \case
  Var _ vname -> vname == name
  LamAbs _ lname term -> lname /= name && mentions name term
  Apply _ f x         -> mentions name f || mentions name x
  Force _ term        -> mentions name term
  Delay _ term        -> mentions name term
  _                        -> False

whnf :: NTerm -> WhnfRes
whnf = whnf' 100

whnf' :: Integer -> NTerm -> WhnfRes
whnf' 0 = const Unclear
whnf' n = let
  rec = whnf' (n-1)
    in \case
  Var{} -> Unclear
  LamAbs{} -> Safe
  Apply _ (LamAbs _ name lTerm) valTerm -> 
    case rec valTerm of
      Err -> Err
      res -> min res $ rec (appBind name valTerm lTerm)
  Apply _ (Apply _ (Builtin _ builtin) arg1) arg2 -> 
    if safe2Arg builtin
       then min (rec arg1) (rec arg2)
       else min Unclear $ min (rec arg1) (rec arg2)
  Apply _ fTerm xTerm -> min Unclear $ min (rec fTerm) (rec xTerm)
    -- it should be possible to make this clear more often
    -- ie. a case over builtins
  Force _ (Delay _ term) -> rec term
  Force{} -> Unclear
  Delay{} -> Safe
  Constant{} -> Safe
  Builtin{} -> Safe
  Error{} -> Err

safe2Arg :: DefaultFun -> Bool
safe2Arg = \case
  AddInteger               -> True
  SubtractInteger          -> True
  MultiplyInteger          -> True
  EqualsInteger            -> True
  LessThanInteger          -> True
  LessThanEqualsInteger    -> True
  AppendByteString         -> True
  ConsByteString           -> True
  IndexByteString          -> True
  EqualsByteString         -> True
  LessThanByteString       -> True
  LessThanEqualsByteString -> True
  VerifySignature          -> True
  AppendString             -> True
  EqualsString             -> True
  ChooseUnit               -> True
  Trace                    -> True
  MkCons                   -> True
  ConstrData               -> True
  EqualsData               -> True
  MkPairData               -> True
  _                        -> False

subTerms :: NTerm -> [(Scope,NTerm)]
subTerms t = (S.empty,t):case t of
                 LamAbs _ n term         -> first (S.insert n) <$> subTerms term
                 Apply _ funTerm varTerm -> subTerms funTerm ++ subTerms varTerm
                 Force _ term            -> subTerms term
                 Delay _ term            -> subTerms term
                 Var{}      -> []
                 Constant{} -> []
                 Builtin{}  -> []
                 Error{}    -> []

unsub :: NTerm -> Name -> NTerm -> NTerm
unsub replacing replaceWith = completeRec $ \case
  term
    | term == replacing -> Just $ Var () replaceWith
  _ -> Nothing

equiv :: (Scope,NTerm) -> (Scope,NTerm) -> Bool
equiv (lscope,lterm) (rscope,rterm)
     = not (uses lscope lterm) 
    && not (uses rscope rterm) 
    && lterm == rterm

  {-
-- compares two (scoped) terms and maybe returns a template the number of nodes of the template and the number of holes in the template
weakEquiv :: (Scope,NTerm) -> (Scope,NTerm) -> Maybe (NTerm,Int,Int)
weakEquiv (lscope,lterm) (rscope,rterm) = do
    guard $ not (uses lscope lterm) 
    guard $ not (uses rscope rterm) 
    weakEquiv' lterm rterm

weakEquiv' :: NTerm -> NTerm -> StateT Int Maybe (NTerm,Int)
weakEquiv' = curry $ \case
  (LamAbs _ ln lt,LamAbs _ rn rt) -> do
    (t,n) <- weakEquiv' lt (subName ln rn rt)
    return (LamAbs () ln t,n,h)
  (Apply _ lf lx,Apply _ rf rx) -> do
    (ft,fnodes,fholes) <- weakEquiv' lf rf
    (xt,xnodes,xholes) <- weakEquiv' lx rx
    return (Apply () ft xt,fnodes+xnodes,fholes+xholes)
  (Delay _ l,Delay _ r) -> first3 (Delay ()) <$> weakEquiv' l r
  (Force _ l,Force _ r) -> first3 (Force ()) <$> weakEquiv' l r
  (l,r) 
    | l == r    -> Just (l,0,0)
    | otherwise -> do
      hn <- get
      modify (+1)
      return $ (Var () (Name{nameString="hole",0)

subName :: Name -> Name -> NTerm -> NTerm
subName replace replaceWith = completeRec $ \case
  LamAbs _ n t -> Just $ LamAbs () (if n == replace then replaceWith else n) (subName replace replaceWith t)
  Var _ n      -> Just $ Var () (if n == replace then replaceWith else n)
  _ -> Nothing
      -}

uses :: Scope -> NTerm -> Bool
uses s = \case
  Apply _ f x -> uses s f || uses s x
  Delay _ t -> uses s t
  Force _ t -> uses s t
  LamAbs _ n t -> n `S.notMember` s && uses s t
  Var _ n -> n `S.member` s 
  _ -> False

newName :: ScopeM Name
newName = do
  n <- get
  modify (+1)
  return $ Name (pack $ show n) (Unique $ fromIntegral n)

-- Tactics

subs :: Tactic
subs = completeTactic $ \case
      Apply _ (LamAbs _ name funTerm) varTerm ->
        case whnf varTerm of
          Safe -> return $ Just [appBind name varTerm funTerm]
          Unclear -> return Nothing
          Err -> return $ Just [Error ()]
      _ -> return Nothing

unsubs :: Tactic
unsubs = completeTactic $ \case
  Apply () funTerm varTerm -> let
    fSubterms = subTerms funTerm
    vSubterms = subTerms varTerm
        in (Just <$>) . sequence $ do
          fSubterm <- fSubterms
          vSubterm <- vSubterms
          guard $ fSubterm `equiv` vSubterm
          return $ do
            name <- newName 
            let funTerm' = unsub (snd fSubterm) name funTerm
                varTerm' = unsub (snd vSubterm) name varTerm
            return $ Apply ()
              (
                LamAbs () name
                  ( Apply () funTerm' varTerm' )
              ) (snd fSubterm)

  _ -> return Nothing

uplcCurry :: Tactic
uplcCurry = completeTactic $ \case
  Apply _
    (LamAbs _ name term)
    (Apply _ (Apply _ (Builtin _ MkPairData) pairFst) pairSnd) -> do
      n1 <- newName 
      n2 <- newName 
      let newTerm =  cleanPairs $ appBind name (Apply () (Apply () (Builtin () MkPairData) (Var () n1)) (Var () n2)) term
      return $ Just [ Apply () (LamAbs () n1 (Apply () (LamAbs () n2 newTerm) pairFst)) pairSnd ]
  _ -> return Nothing

-- Safe Tactics

cleanPairs :: SafeTactic
cleanPairs = completeRec $ \case
  Apply _
    (Builtin _ FstPair)
    (Apply _
      (Apply _
        (Builtin _ MkPairData)
        fstTerm
        )
      _
    ) -> Just $ cleanPairs fstTerm
  Apply _
    (Builtin _ SndPair)
    (Apply _
      (Apply _
        (Builtin _ MkPairData)
        _
        )
      sndTerm
    )
   -> Just $ cleanPairs sndTerm
  _ -> Nothing


removeDeadCode :: SafeTactic
removeDeadCode = completeRec $ \case
  (Apply _ (LamAbs _ name term) val) ->
    case whnf val of
        Safe -> if mentions name term
           then Nothing
           else Just term
        Unclear -> Nothing
        Err -> Just $ Error ()
  _ -> Nothing

