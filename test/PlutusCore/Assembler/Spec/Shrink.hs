{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PlutusCore.Assembler.Spec.Shrink ( tests , testScriptTact) where

import qualified PlutusCore                               as PLC
import           PlutusCore.Assembler.AnnDeBruijn
import           PlutusCore.Assembler.Desugar
import           PlutusCore.Assembler.Parse
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Shrink              (SafeTactic, Tactic,
                                                           Term, removeDeadCode,
                                                           subs)
import           PlutusCore.Assembler.Spec.Gen            (genUplc)
import           PlutusCore.Assembler.Spec.Prelude
import           PlutusCore.Assembler.Tokenize
import           PlutusCore.Default                       (DefaultFun,
                                                           DefaultUni)
import           PlutusCore.Name                          (Name)
import qualified UntypedPlutusCore.Core.Type              as UPLC
import           UntypedPlutusCore.DeBruijn               (Index (..))
import           UntypedPlutusCore.Evaluation.Machine.Cek

import           Control.Monad.Except
import           Control.Monad.State                      (State, evalState,
                                                           get, gets, modify,
                                                           put)
import           Data.Text                                (pack)
import           Prelude                                  (FilePath, Int, error,
                                                           fromIntegral, not,
                                                           print, putStrLn,
                                                           readFile, tail, (!!))

type Result = Either (CekEvaluationException DefaultUni DefaultFun) (UPLC.Term Name DefaultUni DefaultFun ())



tests :: TestTree
tests =
  testGroup "shrinking tactics"
  [ testSafeTactic removeDeadCode
  , testTactic subs
  ]

testSafeTactic :: SafeTactic -> TestTree
testSafeTactic safeTactic = testTactic (return  . safeTactic)

testTactic :: Tactic -> TestTree
testTactic tactic = testProperty "tactics don't break code" . property $ do
  uplc <- forAll genUplc
  let shorts = tactic uplc
  let uplc' = run uplc
  let shorts' = run <$> shorts
  assert $ all (~= uplc') shorts'

(~=) :: Result -> Result -> Bool
a ~= b = case (a,b) of
           (Left _,Left _)   -> True
           (Right _,Right _) -> True
           _                 -> False

(/~=) :: Result -> Result -> Bool
a /~= b = not $ a ~= b

run :: Term -> Result
run = evaluateWithCek . unDeBruijn

unDeBruijn :: Term -> UPLC.Term Name DefaultUni DefaultFun ()
unDeBruijn = (`evalState` ([],names)) . unDeBruijn'
  where
    names = [ PLC.Name (pack . show $ i) (PLC.Unique i) | i <- [1..] ]

unDeBruijn' :: Term -> State ([Name],[Name]) (UPLC.Term Name DefaultUni DefaultFun ())
unDeBruijn' = \case
  UPLC.Var () name -> do
    scope <- gets fst
    let index = deBruijnToInt name
    let name' = scope !! index
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

evaluateWithCek :: UPLC.Term Name DefaultUni DefaultFun () -> Either (CekEvaluationException DefaultUni DefaultFun) (UPLC.Term Name DefaultUni DefaultFun ())
evaluateWithCek = evaluateCekNoEmit PLC.defaultCekParameters

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
                , let res' = run shortening , res /~= res' ]
      putStrLn "bad results:"
      forM_ bad printUplcRes

printUplcRes :: (Term,Result) -> IO ()
printUplcRes (uplc,res) = do
  putStrLn ""
  putStrLn "UPLC"
  print uplc
  putStrLn "unDeBruijn"
  print (unDeBruijn uplc)
  putStrLn "result"
  print res
  putStrLn ""

