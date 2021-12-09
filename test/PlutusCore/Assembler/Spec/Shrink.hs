{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PlutusCore.Assembler.Spec.Shrink ( tests , testScriptTact, unitTest, fromFile, prettyPrintTerm) where

import           Plutus.V1.Ledger.Scripts                 (Script (..))
import qualified Plutus.V1.Ledger.Scripts                 as Scripts
import qualified PlutusCore                               as PLC
import           PlutusCore.Assembler.AnnDeBruijn
import           PlutusCore.Assembler.Assemble            (parseProgram)
import           PlutusCore.Assembler.Desugar
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Shrink              (Program, SafeTactic,
                                                           Tactic, Term,
                                                           defaultShrinkParams,
                                                           safeTactics,
                                                           shrinkProgram, size,
                                                           tactics)
import           PlutusCore.Assembler.Spec.Gen            (genUplc)
import           PlutusCore.Assembler.Spec.Prelude
import           PlutusCore.Assembler.Tokenize
import           PlutusCore.Default                       (DefaultFun,
                                                           DefaultUni)
import           PlutusCore.Evaluation.Machine.ExBudget   (ExBudget (..),
                                                           ExRestrictingBudget (..))
import           PlutusCore.Name                          (Name)
import qualified UntypedPlutusCore.Core.Type              as UPLC
import           UntypedPlutusCore.DeBruijn               (Index (..))
import           UntypedPlutusCore.Evaluation.Machine.Cek

import           Data.Text                                (pack)
import           PlutusCore.Evaluation.Machine.ExMemory   (ExCPU (..),
                                                           ExMemory (..))
import           Prelude                                  (FilePath, curry, not,
                                                           print, putStrLn,
                                                           (++))

type Result = Either (CekEvaluationException DefaultUni DefaultFun) (UPLC.Term Name DefaultUni DefaultFun ())

tests :: TestTree
tests =
  testGroup "shrinking tactics" (
     [ testGroup tactName [ testSafeTactic tact , testSafeTacticShrinks tact ] | (tactName,tact) <- safeTactics defaultShrinkParams ] ++
     [ testGroup tactName [ testTactic     tact ]                              | (tactName,tact) <- tactics     defaultShrinkParams ]
                                )

testSafeTactic :: SafeTactic -> TestTree
testSafeTactic safeTactic = testTactic (return  . safeTactic)

testSafeTacticShrinks :: SafeTactic -> TestTree
testSafeTacticShrinks st = testProperty "Safe tactic doesn't grow code" . property $ do
  uplc <- forAll genUplc
  assert $ size uplc >= size (st uplc)

testTactic :: Tactic -> TestTree
testTactic tactic = testProperty "Tactic doesn't break code" . property $ do
  uplc <- forAll genUplc
  let shorts = tactic uplc
  let uplc' = run uplc
  let shorts' = run <$> shorts
  assert $ all (~= uplc') shorts'

class Similar a where
  (~=) :: a -> a -> Bool

instance Similar (Result,RestrictingSt) where
  (lres,lcost) ~= (rres,rcost) = lres ~= rres && getCpu lcost >= getCpu rcost && getMem lcost >= getMem rcost
    -- Remaining budget must be greater than or equal to unshortened script
    -- it may be reasonable to weaken this in the future if we decide to sacrifice
    -- speed and ram for script size
    -- I should also probably rename the similar class now that it's asymetric
    where
      getCpu (RestrictingSt budget) = exBudgetCPU    . unExRestrictingBudget $ budget
      getMem (RestrictingSt budget) = exBudgetMemory . unExRestrictingBudget $ budget

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


prettyPrintProg :: Program -> String
prettyPrintProg (UPLC.Program () _ term) = prettyPrintTerm term

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

fromFile :: FilePath -> IO (Either ErrorMessage Program)
fromFile fp = do
  txt <- pack <$> readFile fp
  return $ ((desugar . annDeBruijn) <=< parseProgram fp ) txt

testScriptTact :: FilePath -> Tactic -> IO ()
testScriptTact scriptFilePath tact = do
  uplc' <- fromFile scriptFilePath
  case uplc' of
    Left e -> print e
    Right (UPLC.Program () _ uplc) -> do
      let res = run uplc
      printUplcRes (uplc,res)
      let bad = [ (shortening,res')
                | shortening <- tact uplc
                , let res' = run shortening , res' ~/= res ]
      putStrLn "bad results:"
      forM_ bad printUplcRes

printUplcRes :: (Term,(Result,RestrictingSt)) -> IO ()
printUplcRes (uplc,(res,_)) = do
  putStrLn ""
  putStrLn "UPLC"
  print uplc
  --putStrLn "unDeBruijn"
  --print (unDeBruijn uplc)
  putStrLn "result"
  print res
  putStrLn ""

unitTest :: FilePath -> IO ()
unitTest scriptFilePath = do
  uplc' <- fromFile scriptFilePath
  case uplc' of
    Left e -> print e
    Right prog -> do
      let shrink = shrinkProgram prog
      putStrLn $ prettyPrintProg prog
      putStrLn "-------------------"
      putStrLn $ prettyPrintProg shrink

