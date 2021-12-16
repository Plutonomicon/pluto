{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module PlutusCore.Assembler.Spec.Shrink ( makeTests , prettyPrintTerm) where

import           Control.Monad                            (filterM, mapM, (>=>))
import           Data.Either                              (rights)
import           Data.Functor                             ((<&>))
import           Data.List                                (zip)
import           System.Directory                         (doesFileExist,
                                                           listDirectory)
import           System.FilePath                          ((</>))

import           Plutus.V1.Ledger.Scripts                 (Script (..))
import qualified PlutusCore                               as PLC
import           PlutusCore.Assembler.AnnDeBruijn         (annDeBruijn)
import           PlutusCore.Assembler.Assemble            (parseProgram)
import           PlutusCore.Assembler.Desugar             (desugar)
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Shrink              (DTerm, NTerm,
                                                           SafeTactic, Tactic,
                                                           dTermToN,
                                                           defaultShrinkParams,
                                                           safeTactics,
                                                           shrinkDTerm, size,
                                                           tactics)
import           PlutusCore.Assembler.Spec.Gen            (genUplc)
import           PlutusCore.Assembler.Spec.Prelude
import           PlutusCore.Default                       (DefaultFun,
                                                           DefaultUni)
import           PlutusCore.Evaluation.Machine.ExBudget   (ExBudget (..),
                                                           ExRestrictingBudget (..))
import           PlutusCore.Name                          (Name)
import qualified UntypedPlutusCore.Core.Type              as UPLC
import           UntypedPlutusCore.Evaluation.Machine.Cek

import           Data.Text                                (pack)
import           Hedgehog                                 (MonadTest, annotate,
                                                           failure, success)
import           PlutusCore.Evaluation.Machine.ExMemory   (ExCPU (..),
                                                           ExMemory (..))
import           Test.Tasty                               (localOption)
import           Test.Tasty.Hedgehog                      (HedgehogTestLimit (HedgehogTestLimit))

type Result = Either
  (CekEvaluationException DefaultUni DefaultFun)
  (UPLC.Term Name DefaultUni DefaultFun ())

makeTests :: IO TestTree
makeTests = do
  unitTests <- makeUnitTests
  return $ testGroup "shrinking tactics" (
     [ testGroup tactName [ testSafeTactic tactName tact , testSafeTacticShrinks tact ]
       | (tactName,tact) <- safeTactics defaultShrinkParams ] ++
     [ testGroup tactName [ testTactic     tactName tact ]
       | (tactName,tact) <- tactics     defaultShrinkParams ] ++
     [ localOption (HedgehogTestLimit (Just 1)) unitTests ]
                                )

data TacticType = Safe | Unsafe deriving Show

makeUnitTests :: IO TestTree
makeUnitTests = do
  examples' <-  fmap ("./examples" </>) <$> listDirectory "./examples"
  unitTests <-  fmap ("./examples/unitTests" </>) <$> listDirectory "./examples/unitTests"
  examples  <-  filterM doesFileExist (examples' ++ unitTests)
  srcs      <-  mapM (fmap pack . readFile) examples
  let uplcs' = rights [ (name,) <$> ( parseProgram name >=> (fmap Script . desugar . annDeBruijn) $ src )
                      | (name,src) <- zip examples srcs ]
      uplcs  = [(name,uplc) | (name,Script (UPLC.Program _ _ uplc)) <- uplcs' ]
  return $ testGroup "Unit tests" $ (fullTest <$> uplcs) ++ ( do
    (name,uplc) <- uplcs
    (tactName,tact) <- [Safe,Unsafe] >>= \case
        Unsafe -> tactics     defaultShrinkParams
        Safe   -> safeTactics defaultShrinkParams <&> second (return .)
    return $ testProperty ("testing " ++ tactName ++ " on " ++ name) . property $ do
      testTacticOn tactName tact (dTermToN uplc)
                                                            )

fullTest :: (String,DTerm) -> TestTree
fullTest (name,uplc) = testProperty ("full test of shrink on " ++ name) . property $ do
  let uplc' = shrinkDTerm uplc
  assert $ run (dTermToN uplc) ~= run (dTermToN uplc')


testTacticOn :: MonadTest m => String -> Tactic -> NTerm -> m ()
testTacticOn tactName tact uplc = do
  let res = run uplc
  let fails = [ uplc' | uplc' <- tact uplc
              , uplc' /= uplc
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
  uplc <- dTermToN <$> forAll genUplc
  assert $ size uplc >= size (st uplc)

testTactic :: String -> Tactic -> TestTree
testTactic tactName tactic = testProperty "Tactic doesn't break code" . property $ do
  uplc <- forAll genUplc
  testTacticOn tactName tactic (dTermToN uplc)

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
  (~=) = curry $ \case
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

run :: NTerm -> (Result,RestrictingSt)
run = runCekNoEmit PLC.defaultCekParameters ( restricting . ExRestrictingBudget $ ExBudget
    { exBudgetCPU    = 1_000_000_000 :: ExCPU
    , exBudgetMemory = 1_000_000     :: ExMemory
      } )

prettyPrintTerm :: NTerm -> String
prettyPrintTerm = let
    showName n = "V-" ++ show (PLC.unUnique (PLC.nameUnique n))
                   in \case
 UPLC.Var () name -> showName name
 UPLC.LamAbs () name term -> "(\\" ++ showName name ++ "->" ++ prettyPrintTerm term ++ ")"
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

