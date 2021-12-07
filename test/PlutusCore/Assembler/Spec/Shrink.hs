{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PlutusCore.Assembler.Spec.Shrink ( tests , testScriptTact) where

import qualified PlutusCore                               as PLC
import           PlutusCore.Assembler.AnnDeBruijn
import           PlutusCore.Assembler.Desugar
import           PlutusCore.Assembler.Parse
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Shrink              (SafeTactic, Tactic,
                                                           Term,
                                                           defaultShrinkParams,
                                                           safeTactics, size,
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

import           Control.Monad.Except
import           Control.Monad.State                      (State, evalState,
                                                           get, gets, modify,
                                                           put)
import           Data.Text                                (pack)
import           PlutusCore.Evaluation.Machine.ExMemory   (ExCPU (..),
                                                           ExMemory (..))
import           Prelude                                  (FilePath, Int, curry,
                                                           error, fromIntegral,
                                                           length, not, print,
                                                           putStrLn, readFile,
                                                           tail, (!!), (++))

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
run = runWithCek . unDeBruijn

unDeBruijn :: Term -> UPLC.Term Name DefaultUni DefaultFun ()
unDeBruijn = (`evalState` ([],names)) . unDeBruijn'
  where
    names = [ PLC.Name (pack . show $ i) (PLC.Unique i) | i <- [1..] ]

unDeBruijn' :: Term -> State ([Name],[Name]) (UPLC.Term Name DefaultUni DefaultFun ())
unDeBruijn' = \case
  UPLC.Var () name -> do
    scope <- gets fst
    let index = deBruijnToInt name
    let name' = if index >= length scope
                   then error $ "out of scope inex: " ++ show scope ++ " " ++ show index
                   else scope !! index
    return $ UPLC.Var () name'
  UPLC.Force () term -> UPLC.Force () <$> unDeBruijn' term
  UPLC.Delay () term -> UPLC.Delay () <$> unDeBruijn' term
  UPLC.Apply () fTerm xTerm -> UPLC.Apply () <$> unDeBruijn' fTerm <*> unDeBruijn' xTerm
  UPLC.LamAbs () _ term -> do
    name <- scopeName
    term' <- unDeBruijn' term
    modify $ first tail -- unscope the name
    return $ UPLC.LamAbs () name term'
  UPLC.Builtin () b  -> return $ UPLC.Builtin  () b
  UPLC.Constant () c -> return $ UPLC.Constant () c
  UPLC.Error ()      -> return $ UPLC.Error ()

deBruijnToInt :: PLC.DeBruijn -> Int
deBruijnToInt (PLC.DeBruijn (Index n)) = fromIntegral n

scopeName :: State ([Name],[Name]) Name
scopeName = do
  (scope,names) <- get
  case names of
    (new:rest) -> do
      put (new:scope,rest)
      return new
    [] -> error "Unreachable"

runWithCek :: UPLC.Term Name DefaultUni DefaultFun () -> (Result,RestrictingSt)
runWithCek = runCekNoEmit PLC.defaultCekParameters ( restricting . ExRestrictingBudget $ ExBudget
    { exBudgetCPU    = 1_000_000_000 :: ExCPU
    , exBudgetMemory = 1_000_000 :: ExMemory
      } )

testScriptTact :: FilePath -> Tactic -> IO ()
testScriptTact scriptFilePath tact = do
  txt <- pack <$> readFile scriptFilePath
  let uplc' = desugar . annDeBruijn =<< parse =<< tokenize txt
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
  putStrLn "unDeBruijn"
  print (unDeBruijn uplc)
  putStrLn "result"
  print res
  putStrLn ""

