--{-# LANGUAGE DeriveFunctor     #-}
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
  ,stepShrink
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

shrinkProgram :: Program -> Program
shrinkProgram (UPLC.Program ann version term) = UPLC.Program ann version (shrinkTerm term)

shrinkTerm :: Term -> Term
shrinkTerm = runShrink defaultShrinkParams

runShrink :: ShrinkParams -> Term -> Term
runShrink sp = runShrink' sp . return

runShrink' :: ShrinkParams -> [Term] -> Term
runShrink' sp terms = let
  terms' = stepShrink sp terms
     in if size (head terms) > size (head terms')
         then runShrink' sp terms'
         else head terms

stepShrink :: ShrinkParams -> [Term] -> [Term]
stepShrink sp terms = let
  terms' = map (foldl (.) id (snd <$> safeTactics sp)) terms
  in take (parallelTerms sp) $ sortOn size $ do
    tacts <- replicateM (parallelTactics sp) (snd <$> tactics sp)
    foldl (>>=) terms' tacts


size :: Term -> Int
size = fromIntegral . length . serialise . Script . UPLC.Program () (PLC.defaultVersion ())

defaultShrinkParams :: ShrinkParams
defaultShrinkParams = ShrinkParams
  { safeTactics = [("removeDeadCode",removeDeadCode),("clean pairs",cleanPairs)]
  , tactics = [("subs",subs),("curry",curry)] 
  , parallelTactics = 1
  , parallelTerms = 20
  }

-- Utilities to make tactics simpler

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
       UPLC.Apply ann funTerm varTerm -> let
         funTerms = tact funTerm
         varTerms = tact varTerm
          in UPLC.Apply ann funTerm varTerm : 
               [UPLC.Apply ann funTerm' varTerm | funTerm' <- drop 1 funTerms ] 
            ++ [UPLC.Apply ann funTerm varTerm' | varTerm' <- drop 1 varTerms ]
             
         -- runTacticFork tact $ UPLC.Apply ann <$> forkOn funTerm <*> forkOn varTerm
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
whnf = whnf' 100

whnf' :: Int -> Term -> WhnfRes
whnf' 0 = const Unclear
whnf' n = let
  rec = whnf' (n-1)
    in \case
  UPLC.Var{} -> Unclear
  UPLC.LamAbs{} -> Safe
  UPLC.Apply _ (UPLC.LamAbs _ name lTerm) valTerm -> case rec valTerm of
                                                       Err -> Err
                                                       res -> min res $
                                                          rec (appBind name valTerm lTerm)
  UPLC.Apply _ (UPLC.Apply _ (UPLC.Builtin _ builtin) arg1) arg2 -> if safe2Arg builtin
                                                                       then min (rec arg1) (rec arg2)
                                                                       else min Unclear $ min (rec arg1) (rec arg2)
  UPLC.Apply _ fTerm xTerm -> min Unclear $ min (rec fTerm) (rec xTerm)
    -- it should be possible to make this clear more often
    -- ie. a case over builtins
  UPLC.Force _ (UPLC.Delay _ term) -> rec term
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

