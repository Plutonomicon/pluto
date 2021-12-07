{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PlutusCore.Assembler.Shrink
  (shrinkProgram -- all exports besides shrinkProgram are exported for testing
  ,Program
  ,Tactic
  ,SafeTactic
  ,Term
  ,defaultShrinkParams
  ,tactics
  ,safeTactics
  ,size
              )where

import           Codec.Serialise              (serialise)
import           Control.Monad.Reader
import           Data.ByteString.Lazy         (length)
import           Data.List                    (sortOn)
import           Plutus.V1.Ledger.Scripts     (Script (..))
import qualified PlutusCore.Core              as PLC
import           Prelude                      (Int, drop, fromIntegral, head,
                                               id, map, min, take, (++), (>))
import qualified UntypedPlutusCore.Core.Type  as UPLC

import           PlutusCore.Assembler.Prelude hiding (length)
import           PlutusCore.DeBruijn          (DeBruijn (..), Index (..))
import           PlutusCore.Default           (DefaultFun (..), DefaultUni)

type Term    = UPLC.Term    DeBruijn DefaultUni DefaultFun ()
type Program = UPLC.Program DeBruijn DefaultUni DefaultFun ()


type SafeTactic = Term -> Term
-- safe tactics are shortening strategies which
-- can never be counter productive
type Tactic = Term -> [Term]
-- tactics are ways of shortening programs
-- because they can be counter productive
-- they return a list of terms gotten by
-- applying the tactic at different points
-- in the program and the leftmost result
-- is always the one gotten by doing nothing
type PartialTactic = Term -> Maybe [Term]

data ShrinkParams = ShrinkParams
  { safeTactics     :: [(String,SafeTactic)]
  , tactics         :: [(String,Tactic)]
  , parallelTactics :: Int
  , parallelTerms   :: Int
  }
-- Tactics are stored with strings so the tests can
-- name the tactic that failed

data WhnfRes = Err | Unclear  | Safe deriving (Eq,Ord)

data TacticFork a = Pure a | Fork Term (Term -> TacticFork a)
  deriving Functor

-- TacticFork is a free monad used to implement complete

instance Applicative TacticFork where
  pure = Pure
  (Pure f) <*> x   = f <$> x
  (Fork t f) <*> x = Fork t (\t' -> f t' <*> x)

instance Monad TacticFork where
  (Pure x) >>= f   = f x
  (Fork t x) >>= f = Fork t (x >=> f)

shrinkProgram :: Program -> Program
shrinkProgram (UPLC.Program ann version term) = UPLC.Program ann version (shrinkTerm term)

shrinkTerm :: Term -> Term
shrinkTerm = runShrink defaultShrinkParams

runShrink :: ShrinkParams -> Term -> Term
runShrink sp = runShrink' sp . return

runShrink' :: ShrinkParams -> [Term] -> Term
runShrink' sp terms = let
  terms1 = map (foldl (.) id (snd <$> safeTactics sp)) terms
  terms2 = sortOn size $ do
    tacts <- replicateM (parallelTactics sp) (snd <$> tactics sp)
    foldl (>>=) terms1 tacts
    in if size (head terms) > size (head terms2)
           then runShrink' sp (take (parallelTerms sp) terms2)
           else head terms

size :: Term -> Int
size = fromIntegral . length . serialise . Script . UPLC.Program () (PLC.defaultVersion ())

defaultShrinkParams :: ShrinkParams
defaultShrinkParams = ShrinkParams
  { safeTactics = [("removeDeadCode",removeDeadCode),("clean pairs",cleanPairs)]
  , tactics = [("subs",subs),("curry",curry)] -- subs
  , parallelTactics = 1
  , parallelTerms = 20
  }

-- Utilities to make tactics simpler

runTacticFork :: Tactic -> TacticFork Term -> [Term]
runTacticFork _ (Pure term) = return term
runTacticFork tact (Fork term cont) =
  [ runTacticFork' (cont term') | term' <- tact term ]
  ++ drop 1 (runTacticFork tact (cont term) )
  -- This drop 1 removes the duplicate of the unaltered term
  -- this requires that the original term is always
  -- the head. complete enforces this by always postpend new terms

runTacticFork' :: TacticFork Term -> Term
runTacticFork' (Pure term)      = term
runTacticFork' (Fork term cont) = runTacticFork' $ cont term

forkOn :: Term -> TacticFork Term
forkOn t = Fork t return

completeTactic :: PartialTactic -> Tactic
completeTactic pt term = let
  tact = completeTactic pt
    in case pt term of
        Just terms -> descend tact term ++ terms
        Nothing    -> descend tact term

descend :: Tactic -> Tactic
descend tact = \case
       UPLC.Var ann name -> return $ UPLC.Var ann name
       UPLC.LamAbs ann name term -> UPLC.LamAbs ann name <$> tact term
       UPLC.Apply ann funTerm varTerm ->
         runTacticFork tact $ UPLC.Apply ann <$> forkOn funTerm <*> forkOn varTerm
       UPLC.Force ann term -> UPLC.Force ann <$> tact term
       UPLC.Delay ann term -> UPLC.Delay ann <$> tact term
       UPLC.Constant ann val -> return $ UPLC.Constant ann val
       UPLC.Builtin ann fun  -> return $ UPLC.Builtin ann fun
       UPLC.Error ann -> return $ UPLC.Error ann

completeRec :: (Term -> Maybe Term) -> Term -> Term
completeRec partial originalTerm = let
  rec = completeRec partial
    in case partial originalTerm of
      Just term -> term
      Nothing ->
        case originalTerm of
          UPLC.LamAbs ann name term -> UPLC.LamAbs ann name (rec term)
          UPLC.Apply ann f x        -> UPLC.Apply ann (rec f) (rec x)
          UPLC.Force ann term       -> UPLC.Force ann (rec term)
          UPLC.Delay ann term       -> UPLC.Delay ann (rec term)
          term                      -> term

appBind :: DeBruijn -> Term -> Term -> Term
appBind name val = completeRec $ \case
      UPLC.Var _ varName -> if dbnIndex name == dbnIndex varName
                               then Just val
                               else Nothing
      UPLC.LamAbs ann lname term -> Just $ UPLC.LamAbs ann lname (appBind (incName name) (incDeBruijns val) term)
      _ -> Nothing

incName :: DeBruijn -> DeBruijn
incName (DeBruijn n) = DeBruijn (n+1)

incDeBruijns :: Term -> Term
incDeBruijns = incDeBruijns' 0

incDeBruijns' :: Index -> Term -> Term
incDeBruijns' level = completeRec $ \case
  UPLC.Var ann name -> Just $ UPLC.Var ann (incAbove level name)
  UPLC.LamAbs ann name term -> Just $ UPLC.LamAbs ann name (decDeBruijns' (level + 1) term)
  _                 -> Nothing

decAbove :: Index -> DeBruijn -> DeBruijn
decAbove level (DeBruijn n) = if n > level then DeBruijn (n-1) else DeBruijn n

incAbove :: Index -> DeBruijn -> DeBruijn
incAbove level (DeBruijn n) = if n > level then DeBruijn (n+1) else DeBruijn n

decDeBruijns :: Term -> Term
decDeBruijns = decDeBruijns' 0
  ,Program

decDeBruijns' :: Index -> Term -> Term
decDeBruijns' level = completeRec $ \case
  UPLC.Var ann name -> Just $ UPLC.Var ann (decAbove level name)
  UPLC.LamAbs ann name term -> Just $ UPLC.LamAbs ann name (decDeBruijns' (level + 1) term)
  _                 -> Nothing

mentions :: DeBruijn -> Term -> Bool
mentions name@(DeBruijn n) = \case
  UPLC.Var _ (DeBruijn vn) -> vn == n
  UPLC.LamAbs _ _ term     -> mentions (incName name) term
  UPLC.Apply _ f x         -> mentions name f || mentions name x
  UPLC.Force _ term        -> mentions name term
  UPLC.Delay _ term        -> mentions name term
  _                        -> False

whnf :: Term -> WhnfRes
whnf = \case
  UPLC.Var{} -> Unclear
  UPLC.LamAbs{} -> Safe
  UPLC.Apply _ (UPLC.LamAbs _ name lTerm) valTerm -> case whnf valTerm of
                                                       Err -> Err
                                                       res -> min res $
                                                          whnf (appBind name valTerm lTerm)
  UPLC.Apply _ (UPLC.Apply _ (UPLC.Builtin _ builtin) arg1) arg2 -> if safe2Arg builtin
                                                                       then min (whnf arg1) (whnf arg2)
                                                                       else min Unclear $ min (whnf arg1) (whnf arg2)
  UPLC.Apply _ fTerm xTerm -> min Unclear $ min (whnf fTerm) (whnf xTerm)
    -- it should be possible to make this clear more often
    -- ie. a case over builtins
  UPLC.Force _ (UPLC.Delay _ term) -> whnf term
  UPLC.Force{} -> Unclear
  UPLC.Delay{} -> Safe
  UPLC.Constant{} -> Safe
  UPLC.Builtin{} -> Safe
  UPLC.Error{} -> Err

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

-- Tactics

subs :: Tactic
subs = completeTactic $ \case
      UPLC.Apply _ (UPLC.LamAbs _ name funTerm) varTerm ->
        case whnf varTerm of
          Safe -> return . return $ decDeBruijns $ appBind name varTerm funTerm
          Unclear -> Nothing
          Err -> return . return $ UPLC.Error ()
      _ -> Nothing

curry :: Tactic
curry = completeTactic $ \case
  UPLC.Apply _
    (UPLC.LamAbs _ name term)
    (UPLC.Apply _ (UPLC.Apply _ (UPLC.Builtin _ MkPairData) pairFst) pairSnd)
      -> let
            newTerm = cleanPairs $ appBind name
              (UPLC.Apply () (UPLC.Apply () (UPLC.Builtin () MkPairData)
                (UPLC.Var () (DeBruijn $ Index 0)))
                (UPLC.Var () (DeBruijn $ Index 1)))
                $ decDeBruijns term
            in return . return $
                    UPLC.Apply ()
                      (UPLC.LamAbs ()
                        (DeBruijn $ Index 0)
                        (UPLC.Apply ()
                          (UPLC.LamAbs ()
                            (DeBruijn $ Index 0)
                            newTerm
                          )
                          pairFst
                        )
                      ) pairSnd
  _ -> Nothing

-- Safe Tactics

cleanPairs :: SafeTactic
cleanPairs = completeRec $ \case
  UPLC.Apply _
    (UPLC.Builtin _ FstPair)
    (UPLC.Apply _
      (UPLC.Apply _
        (UPLC.Builtin _ MkPairData)
        fstTerm
        )
      _
    ) -> Just $ cleanPairs fstTerm
  UPLC.Apply _
    (UPLC.Builtin _ SndPair)
    (UPLC.Apply _
      (UPLC.Apply _
        (UPLC.Builtin _ MkPairData)
        _
        )
      sndTerm
    )
   -> Just $ cleanPairs sndTerm
  _ -> Nothing


removeDeadCode :: SafeTactic
removeDeadCode = completeRec $ \case
  (UPLC.Apply _ (UPLC.LamAbs _ name term) val) ->
    case whnf val of
        Safe -> if mentions name term
           then Nothing
           else Just $ decDeBruijns term
        Unclear -> Nothing
        Err -> Just $ UPLC.Error ()
  _ -> Nothing

