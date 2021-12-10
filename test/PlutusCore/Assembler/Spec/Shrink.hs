{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module PlutusCore.Assembler.Spec.Shrink ( tests , prettyPrintTerm) where

import           Control.Monad                            (mapM, filterM, (>=>))
import           Data.Either                              (rights)
import           Data.List                                (lookup,zip)
import           Data.Maybe                               (fromJust)
import           System.Directory                         (listDirectory,doesFileExist)
import           System.FilePath                          ((</>))

import           Plutus.V1.Ledger.Scripts                 (Script (..))
import qualified Plutus.V1.Ledger.Scripts                 as Scripts
import qualified PlutusCore                               as PLC
import           PlutusCore.Assembler.Assemble            (parseProgram,
                                                           translate)
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Shrink              (SafeTactic,
                                                           Tactic, Term,
                                                           defaultShrinkParams,
                                                           safeTactics,
                                                           size, tactics)
import           PlutusCore.Assembler.Spec.Gen            (genUplc)
import           PlutusCore.Assembler.Spec.Prelude
import           PlutusCore.Default                       (DefaultFun,
                                                           DefaultUni)
import           PlutusCore.Evaluation.Machine.ExBudget   (ExBudget (..),
                                                           ExRestrictingBudget (..))
import           PlutusCore.Name                          (Name)
import qualified UntypedPlutusCore.Core.Type              as UPLC
import           UntypedPlutusCore.DeBruijn               (Index (..))
import           UntypedPlutusCore.Evaluation.Machine.Cek

import           Data.Text                                (pack)
import           Hedgehog                                 (MonadTest, annotate,
                                                           failure, success)
import qualified Hedgehog.Gen                             as Gen
import           PlutusCore.Evaluation.Machine.ExMemory   (ExCPU (..),
                                                           ExMemory (..))
import           Test.Tasty                               (localOption)
import           Test.Tasty.Hedgehog                      (HedgehogTestLimit (..))

type Result = Either
  (CekEvaluationException DefaultUni DefaultFun)
  (UPLC.Term Name DefaultUni DefaultFun ())

tests :: TestTree
tests =
  testGroup "shrinking tactics" (
     [ localOption (HedgehogTestLimit Nothing) exampleUnitTests ] ++
     [ testGroup tactName [ testSafeTactic tactName tact , testSafeTacticShrinks tact ]
     | (tactName,tact) <- safeTactics defaultShrinkParams ] ++
     [ testGroup tactName [ testTactic     tactName tact ]
     | (tactName,tact) <- tactics     defaultShrinkParams ] 
                                )

data TacticType = Safe | Unsafe deriving Show

exampleUnitTests :: TestTree
exampleUnitTests = testProperty "tactics don't break examples (generally a slow test)" . property $ do
  examples' <- liftIO $ fmap ("./examples" </>) <$> listDirectory "./examples"
  unitTests <- liftIO $ fmap ("./examples/unitTests" </>) <$> listDirectory "./examples/unitTests"
  examples  <- liftIO $ filterM doesFileExist (examples' ++ unitTests)
  srcs     <- liftIO $ mapM (fmap pack . readFile) examples
  let uplcs' = rights [ (name,) <$> ( parseProgram name >=> translate $ src )
                      | (name,src) <- zip examples srcs ]
      uplcs  = [(name,uplc) | (name,Script (UPLC.Program _ _ uplc)) <- uplcs' ]
  (exampleName,uplc) <- forAll $ Gen.choice (return <$> uplcs)
  (tactName,tact) <- forAll (Gen.choice (return <$> [Safe,Unsafe])) >>= \case
    Unsafe -> do
      tactName <- forAll $ Gen.choice [ return name | (name,_) <- tactics defaultShrinkParams ]
      let tact = fromJust $ lookup tactName (tactics defaultShrinkParams)
      return (tactName,tact)
    Safe -> do
      tactName <- forAll $ Gen.choice [ return name | (name,_) <- safeTactics defaultShrinkParams ]
      let tact = fromJust $ lookup tactName (safeTactics defaultShrinkParams)
      return (tactName,return . tact)

  annotate $ exampleName ++ " was broken by " ++ tactName
  testTacticOn tactName tact uplc

testTacticOn :: MonadTest m => String -> Tactic -> Term -> m ()
testTacticOn tactName tact uplc = do
  let res = run uplc
  let asScript = Script . UPLC.Program () (UPLC.Version () 0 0 0)
  let fails = [ uplc' | uplc' <- tact uplc
              , asScript uplc' /= asScript uplc
              , run uplc' ~/= res ]
  case fails of
    [] -> success
    ( bad : _) -> do
      annotate $ prettyPrintTerm uplc
      annotate $ "produced: " ++  show res
      annotate $ "Shrank by " ++ tactName ++ " to"
      annotate $ prettyPrintTerm bad
      annotate $ "produced: " ++  show (run bad)
      failure


testSafeTactic :: String -> SafeTactic -> TestTree
testSafeTactic tactName safeTactic = testTactic tactName (return  . safeTactic)

testSafeTacticShrinks :: SafeTactic -> TestTree
testSafeTacticShrinks st = testProperty "Safe tactic doesn't grow code" . property $ do
  uplc <- forAll genUplc
  assert $ size uplc >= size (st uplc)

testTactic :: String -> Tactic -> TestTree
testTactic tactName tactic = testProperty "Tactic doesn't break code" . property $ do
  uplc <- forAll genUplc
  testTacticOn tactName tactic uplc

class Similar a where
  (~=) :: a -> a -> Bool

instance Similar (Result,RestrictingSt) where
  (lres,lcost) ~= (rres,rcost) = lres ~= rres && getCpu lcost ~= getCpu rcost && getMem lcost ~= getMem rcost
    where
      getCpu (RestrictingSt budget) = exBudgetCPU    . unExRestrictingBudget $ budget
      getMem (RestrictingSt budget) = exBudgetMemory . unExRestrictingBudget $ budget

-- For now I similar test that cpu and memory changes are not too signifigant

instance Similar ExCPU where
  a ~= b = 5 * abs (a-b) < abs a+abs b

instance Similar ExMemory where
  a ~= b = 5 * abs (a-b) < abs a+abs b

instance Similar Result where
  a ~= b = case (a,b) of
           (Left _,Left _)             -> True
           (Right lValue,Right rValue) -> lValue ~= rValue
           _                           -> False

instance Similar (UPLC.Term Name DefaultUni DefaultFun ()) where
  (~=) = curry $ \case
       (UPLC.Var      () _    ,UPLC.Var      () _    ) -> True
       (UPLC.Force    () a    ,UPLC.Force    () b    ) -> a ~= b
       (UPLC.Delay    () a    ,UPLC.Delay    () b    ) -> a ~= b
       (UPLC.Apply () _ _     ,_                     ) -> True
       (_                     ,UPLC.Apply () _ _     ) -> True
       (UPLC.LamAbs   () _ _  ,UPLC.LamAbs   () _ _  ) -> True
       (UPLC.Builtin  () a    ,UPLC.Builtin  () b    ) -> a == b
       (UPLC.Constant () a    ,UPLC.Constant () b    ) -> a == b
       (UPLC.Error    ()      ,UPLC.Error    ()      ) -> True
       _                                               -> False

(~/=) :: Similar a => a -> a -> Bool
a ~/= b = not $ a ~= b

run :: Term -> (Result,RestrictingSt)
run = runWithCek . grabTerm . Scripts.mkTermToEvaluate . Script . UPLC.Program () (UPLC.Version () 0 0 0)
  where
    grabTerm (Right (UPLC.Program _ _ term)) = term
    grabTerm (Left err)                      = error $ show err

runWithCek :: UPLC.Term Name DefaultUni DefaultFun () -> (Result,RestrictingSt)
runWithCek = runCekNoEmit PLC.defaultCekParameters ( restricting . ExRestrictingBudget $ ExBudget
    { exBudgetCPU    = 1_000_000_000 :: ExCPU
    , exBudgetMemory = 1_000_000     :: ExMemory
      } )

prettyPrintTerm :: Term -> String
prettyPrintTerm = \case
 UPLC.Var () (PLC.DeBruijn (Index i)) -> "V" ++ show i
 UPLC.LamAbs () (PLC.DeBruijn (Index 0)) term -> "(\\" ++ prettyPrintTerm term ++ ")"
 UPLC.LamAbs () (PLC.DeBruijn (Index i)) _ -> error $ "bad DeBruijn index" ++ show i
 UPLC.Apply () f@(UPLC.LamAbs{}) x -> prettyPrintTerm f ++ " (" ++ prettyPrintTerm x ++ ")"
 UPLC.Apply () f x -> "(" ++ prettyPrintTerm f ++ ") (" ++ prettyPrintTerm x ++ ")"
 UPLC.Force () term -> "!(" ++ prettyPrintTerm term ++ ")"
 UPLC.Delay () term -> "#(" ++ prettyPrintTerm term ++ ")"
 UPLC.Constant () (PLC.Some (PLC.ValueOf ty con)) -> case (ty,con) of
                                             (PLC.DefaultUniInteger    ,i   ) -> show i
                                             (PLC.DefaultUniByteString ,bs  ) -> show bs
                                             (PLC.DefaultUniString     ,txt ) -> show txt
                                             (PLC.DefaultUniUnit       ,()  ) -> "()"
                                             (PLC.DefaultUniBool       ,b   ) -> show b
                                             (PLC.DefaultUniData       ,dat ) -> show dat
                                             _ -> "Exotic constant"
 UPLC.Builtin () f -> show f
 UPLC.Error () -> "Error"

