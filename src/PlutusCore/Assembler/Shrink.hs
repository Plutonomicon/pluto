{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

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


import           Codec.Serialise              (serialise)
import           Control.Monad                (join, liftM2, replicateM,
                                               sequence)
import           Control.Monad.Reader         (MonadReader, ReaderT, ask, local,
                                               runReaderT)
import           Control.Monad.State          (MonadState, State, StateT,
                                               evalStateT, get, modify, put,
                                               runState, runStateT)
import           Data.ByteString.Lazy         (toStrict)
import           Data.Function                (on)
import           Data.Functor                 ((<&>))
import           Data.Functor.Identity        (runIdentity)
import           Data.List                    (and, elem, filter, foldr, group,
                                               groupBy, maximum, notElem,
                                               sortOn)
import qualified Data.Map                     as M
import           Data.Maybe                   (fromMaybe)
import           Data.Set                     (Set)
import qualified Data.Set                     as S
import           Data.Text                    (pack)
import           Plutus.V1.Ledger.Scripts     (Script (Script),
                                               fromCompiledCode, scriptSize)
import           PlutusCore                   (FreeVariableError, runQuoteT)
import           PlutusCore.Assembler.Prelude
import           PlutusCore.DeBruijn          (DeBruijn, fakeNameDeBruijn)
import           PlutusCore.Default           (DefaultFun (..), DefaultUni)
import           PlutusTx.Code                (CompiledCode,
                                               CompiledCodeIn (DeserializedCode, SerializedCode))
import           UntypedPlutusCore            (Name (Name, nameUnique),
                                               Unique (Unique, unUnique),
                                               Version (Version), deBruijnTerm,
                                               programMapNames, termMapNames,
                                               unDeBruijnTerm, unNameDeBruijn)
import           UntypedPlutusCore.Core.Type  (Program (Program),
                                               Term (Apply, Builtin, Constant, Delay, Error, Force, LamAbs, Var))

type DTerm    = Term    DeBruijn DefaultUni DefaultFun ()
type NTerm    = Term    Name     DefaultUni DefaultFun ()
type DProgram = Program DeBruijn DefaultUni DefaultFun ()

type Scope = Set Name
type ScopeMT m = ReaderT (Scope,Scope) (StateT Integer m)
type ScopeM    = ReaderT (Scope,Scope) (State  Integer  )

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

class (MonadReader (Scope,Scope) m,MonadState Integer m) => MonadScope m where

instance Monad m => MonadScope (ScopeMT m) where

runScopeMT :: Monad m => NTerm -> ScopeMT m a -> m a
runScopeMT nt smtma = let
  globalScope = usedScope nt
  free = fromIntegral$1+maximum (0:(unUnique . nameUnique <$> S.toList globalScope))
    in evalStateT (runReaderT smtma (globalScope,S.empty)) free

runScopeM :: NTerm -> ScopeM a -> a
runScopeM nt = runIdentity . runScopeMT nt

usedScope :: NTerm -> Scope
usedScope = \case
  Var _ n      -> S.singleton n
  LamAbs _ n t -> S.insert n (usedScope t)
  Force _ t    -> usedScope t
  Delay _ t    -> usedScope t
  Apply _ f x  -> S.union (usedScope f) (usedScope x)
  _            -> S.empty

liftScope :: Monad m => ScopeM a -> ScopeMT m a
liftScope sma = do
  s <- ask
  f <- get
  let (a,f') = runState (runReaderT sma s) f
  put f'
  return a

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
  Left s  -> error $ "nTermToD failed with" ++ show (s :: FreeVariableError)
  ) . runQuoteT . deBruijnTerm

dTermToN :: DTerm -> NTerm
dTermToN = (\case
  Right t -> t
  Left s  -> error $ "dTermToN failed with" ++ show (s :: FreeVariableError)
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
  | sizeLast > sizeNow = runShrink (extraSteps sp) sp terms'
  | es > 0             = runShrink (es -1)         sp terms'
  | otherwise          = head terms
    where
      sizeNow  = size $ head terms
      sizeLast = size $ head terms'
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
  , tactics = [("subs",subs),("unsubs",unsubs),("curry",uplcCurry),("strongUnsubs",strongUnsubs)]
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

-- TODO rewrite this in terms of completeRecScope
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

completeRecScope :: (NTerm -> ScopeM (Maybe NTerm)) -> NTerm -> ScopeM NTerm
completeRecScope partial originalTerm = let
  rec = completeRecScope partial
    in partial originalTerm >>= \case
      Just term -> return term
      Nothing ->
        case originalTerm of
          LamAbs _ name term -> LamAbs () name <$> rec term
          Apply  _ f x       -> Apply  () <$> rec f <*> rec x
          Force  _ term      -> Force  () <$> rec term
          Delay  _ term      -> Delay  () <$> rec term
          term               -> return term

appBind :: Name -> NTerm -> NTerm -> NTerm
appBind name val = completeRec $ \case
      Var _ varName -> if name == varName
                               then Just val
                               else Nothing
      _ -> Nothing

mentions :: Name -> NTerm -> Bool
mentions name = \case
  Var _ vname         -> vname == name
  LamAbs _ lname term -> lname /= name && mentions name term
  Apply _ f x         -> mentions name f || mentions name x
  Force _ term        -> mentions name term
  Delay _ term        -> mentions name term
  _                   -> False

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
                 Var{}                   -> []
                 Constant{}              -> []
                 Builtin{}               -> []
                 Error{}                 -> []

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

-- compares two (scoped) terms and maybe returns a template
-- the number of nodes of the template and the holes in the template
weakEquiv :: (Scope,NTerm) -> (Scope,NTerm) -> ScopeMT Maybe (NTerm,Integer,[Name])
weakEquiv (lscope,lterm) (rscope,rterm) = do
    -- ensure that unshared scope is not used
    guard $ not (uses lscope lterm)
    guard $ not (uses rscope rterm)
    weakEquiv' lterm rterm

weakEquiv' :: NTerm -> NTerm -> ScopeMT Maybe (NTerm,Integer,[Name])
weakEquiv' = curry $ \case
  (LamAbs _ ln lt,LamAbs _ rn rt)
    | ln == rn -> do
      (t,n,hs) <- weakEquiv' lt rt
      return (LamAbs () ln t,n,hs)
    | otherwise -> do
      rt' <- subName ln rn rt
      (t,n,hs) <- weakEquiv' lt rt'
      return (LamAbs () ln t,n,hs)
  (Apply _ lf lx,Apply _ rf rx) -> do
    (ft,fnodes,fholes) <- weakEquiv' lf rf
    (xt,xnodes,xholes) <- weakEquiv' lx rx
    return (Apply () ft xt,fnodes+xnodes,fholes++xholes)
  (Delay _ l,Delay _ r) -> do
    (t,n,h) <- weakEquiv' l r
    return (Delay () t,n+1,h)
  (Force _ l,Force _ r) -> do
    (t,n,h) <- weakEquiv' l r
    return (Force () t,n+1,h)
  (l,r)
    | l == r    -> return (l,1,[])
    | otherwise -> do
        holeName <- newName
        return (Var () holeName,1,[holeName])

subName :: MonadScope m => Name -> Name -> NTerm -> m NTerm
subName replace replaceWith term = do
  new <- newName
  return $ subName' replace replaceWith $ subName' replaceWith new term

subName' :: Name -> Name -> NTerm -> NTerm
subName' replace replaceWith = completeRec $ \case
  LamAbs _ n t -> Just $ LamAbs () (if n == replace then replaceWith else n) (subName' replace replaceWith t)
  Var _ n      -> Just $ Var () (if n == replace then replaceWith else n)
  _ -> Nothing

uses :: Scope -> NTerm -> Bool
uses s = \case
  Apply _ f x  -> uses s f || uses s x
  Delay _ t    -> uses s t
  Force _ t    -> uses s t
  LamAbs _ n t -> n `S.notMember` s && uses s t
  Var _ n      -> n `S.member` s
  _            -> False

newName :: MonadScope m => m Name
newName = do
  n <- get
  modify (+1)
  return $ Name (pack $ show n) (Unique $ fromIntegral n)

-- Tactics

subs :: Tactic
subs = completeTactic $ \case
      Apply _ (LamAbs _ name funTerm) varTerm ->
        case whnf varTerm of
          Safe    -> return $ Just [appBind name varTerm funTerm]
          Unclear -> return Nothing
          Err     -> return $ Just [Error ()]
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

strongUnsubs :: Tactic
strongUnsubs = completeTactic $ \case
  Apply () funTerm varTerm -> let
    fSubterms = subTerms funTerm
    vSubterms = subTerms varTerm
      in sepMaybe $ sequence $ do
        fSubterm <- fSubterms
        vSubterm <- vSubterms
        return $ do
          (template,nodes,holes) <- weakEquiv fSubterm vSubterm
          guard $ nodes > 3 -- any less than this is probably unproductive to work with
          name <- newName
          funTerm' <- liftScope $ withTemplate name (template,holes) (snd fSubterm)
          varTerm' <- liftScope $ withTemplate name (template,holes) (snd vSubterm)
          let templateArg = makeLambs holes template
          return $ Apply ()
            (
              LamAbs () name
                ( Apply () funTerm' varTerm' )
            ) templateArg
  _ -> return Nothing

sepMaybe :: ScopeMT Maybe a -> ScopeM (Maybe a)
sepMaybe smtma = do
  s <- ask
  f <- get
  case runStateT (runReaderT smtma s) f of
    Just (a,f') -> put f' >> return (Just a)
    Nothing     -> return Nothing

makeLambs :: [Name] -> NTerm -> NTerm
makeLambs = flip $ foldr (LamAbs ())


withTemplate :: Name -> (NTerm,[Name]) -> NTerm -> ScopeM NTerm
withTemplate templateName (template,holes) = completeRecScope $ \target -> do
  args <- findHoles holes template target
  return $ applyArgs (Var () templateName) . M.elems <$> args

findHoles :: [Name] -> NTerm -> NTerm -> ScopeM (Maybe (Map Name NTerm))
findHoles holes template subTerm
  | template == subTerm = return $ Just M.empty
  | otherwise = case (template,subTerm) of
    (Var () nt,st)
      | nt `elem` holes -> return $ Just $ M.singleton nt st
    (Force _ t,Force _ s)  -> findHoles holes t s
    (Delay _ t,Delay _ s)  -> findHoles holes t s
    (LamAbs _ tn tt,LamAbs _ sn st)
      | tn == sn -> findHoles holes tt st
      | otherwise -> do
        st' <- subName tn sn st
        findHoles holes tt st'
    (Apply _ tf tx,Apply _ sf sx) -> join <$> liftM2 (liftM2 reconsile)
      (findHoles holes tf sf)
      (findHoles holes tx sx)
    _ -> return Nothing

reconsile :: (Ord k,Eq a) => Map k a -> Map k a -> Maybe (Map k a)
reconsile m1 m2 = do
  guard $ and $ M.intersectionWith (==) m1 m2
  return $ M.union m1 m2

applyArgs :: NTerm -> [NTerm] -> NTerm
applyArgs = foldl (Apply ())


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

