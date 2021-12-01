{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}


module PlutusCore.Assembler.EntryPoint (main) where


import           Data.Text                               (pack, unpack)
import qualified Options.Applicative                     as O
import           System.IO                               (FilePath, getContents,
                                                          hPutStrLn, print,
                                                          putStrLn, readFile,
                                                          stderr, writeFile)
import           Text.Hex                                (encodeHex)

import qualified Data.Text                               as T
import qualified Plutus.V1.Ledger.Scripts                as Scripts
import qualified PlutusCore.Assembler.Assemble           as Assemble
import qualified PlutusCore.Assembler.Evaluate           as Evaluate
import           PlutusCore.Assembler.Prelude
import qualified PlutusCore.Assembler.Transform          as Transform
import qualified PlutusCore.Assembler.Types.AST          as AST
import           PlutusCore.Assembler.Types.ErrorMessage (ErrorMessage (..))
import qualified PlutusCore.Pretty                       as Pretty
import qualified Shower


newtype InputFilePath = InputFilePath { getInputFilePath :: FilePath }


newtype OutputFilePath = OutputFilePath { _getOutputFilePath :: FilePath }


data Command
  = CommandAssemble
    (Maybe InputFilePath)
    (Maybe OutputFilePath)
  | CommandRun
    (Maybe InputFilePath)
  | CommandEval
    (Maybe InputFilePath)
    AST.Name
    [AST.Term ()]


inputFilePath :: O.Parser (Maybe InputFilePath)
inputFilePath =
  O.argument (Just . InputFilePath <$> O.str) (O.metavar "INPUT" <> O.value Nothing <> O.help "The input file path: defaults to stdin")


outputFilePath :: O.Parser (Maybe OutputFilePath)
outputFilePath =
  O.argument (Just . OutputFilePath <$> O.str) (O.metavar "OUTPUT" <> O.value Nothing <> O.help "The output file path: defaults to stdout")


command :: O.Parser Command
command =
  O.subparser . mconcat $
    [ O.command "assemble" (O.info assembleP (O.progDesc "Assemble to Plutus bytecode, and display the HEX")),
      O.command "run" (O.info runP (O.progDesc "Run the Pluto code in Plutus evauator")),
      O.command "eval" (O.info runBindingP (O.progDesc "Evaluate a let binding in the program"))
    ]
  where
    assembleP =
      CommandAssemble
      <$> inputFilePath
      <*> outputFilePath
    runP =
      CommandRun
      <$> inputFilePath
    runBindingP =
      CommandEval
      <$> inputFilePath
      <*> fmap AST.Name (O.strArgument (O.metavar "BINDING" <> O.help "Name of the variable bound in the let block"))
      <*> O.many (O.argument termReader (O.metavar "ARG"))
    termReader :: O.ReadM (AST.Term ())
    termReader =
      O.eitherReader $ \(T.pack -> s) ->
        bimap (T.unpack . getErrorMessage) (void . AST.unProgram) $ Assemble.parseProgram "<cli-arg>" s


commandInfo :: O.ParserInfo Command
commandInfo =
  O.info (command O.<**> O.helper)
   ( O.fullDesc
  <> O.progDesc "Pluto assembles into Plutus Core bytecode"
  <> O.header "pluto - Untyped Plutus Core assembler"
   )

-- | An error when running the CLI command.
--
-- Gathers all possible errors in the application.
data Error
  = ErrorParsing ErrorMessage
  | ErrorAssembling ErrorMessage
  | ErrorEvaluating Evaluate.ScriptError
  | ErrorOther Text
  deriving (Eq, Show)

logInfo :: MonadIO m => Text -> m ()
logInfo s =
  liftIO $ putStrLn $ T.unpack s

-- | Like `logInfo` but displays the value pretty-printed using shower.
logShower :: (MonadIO m, Show a) => a -> m ()
logShower =
  logInfo . T.pack . Shower.shower

logError :: MonadIO m => Error -> m ()
logError = \case
  ErrorParsing (ErrorMessage em)    -> f (Just "parser") em
  ErrorAssembling (ErrorMessage em) -> f (Just "assembler") em
  ErrorEvaluating em                -> f (Just "eval") (T.pack $ show em)
  ErrorOther em                     -> f Nothing em
  where
    f mName msg = do
      let prefix = maybe "Error" (\name -> "Error(" <> name <> ")") mName
      liftIO $ hPutStrLn stderr $ prefix <> ": " <> T.unpack msg


runCommand :: forall m. (MonadIO m, MonadError Error m) => Command -> m ()
runCommand cmd = do
  case cmd of
    CommandAssemble mInPath mOutPath -> do
      bin <- assembleInput mInPath
      writeObjectCode mOutPath bin
    CommandRun mInPath -> do
      ast <- parseInput mInPath
      -- TODO: Depending on the need, enable/disable individiual dumps in CLI arguments.
      logHeader "AST"
      logShower $ void ast
      prog <- liftError ErrorAssembling $ Assemble.translate ast
      logHeader "UPLC"
      logShower $ Scripts.unScript prog
      logHeader "UPLC (pretty)"
      logInfo $ Pretty.display $ Scripts.unScript prog
      (exBudget, traces, res) <- liftError ErrorEvaluating $ Evaluate.eval prog
      logHeader "ExBudget"
      liftIO $ print exBudget
      logHeader "Script traces"
      forM_ traces $ \trace ->
        logInfo trace
      logHeader "Script result"
      liftIO $ print res
    CommandEval mInPath name args -> do
      ast <- void <$> parseInput mInPath
      ast' <- liftError ErrorOther $ Transform.applyToplevelBinding name args ast
      logHeader "AST (transformed)"
      logShower ast'
      script <- liftError ErrorAssembling $ Assemble.translate ast'
      (_, _, res) <- liftError ErrorEvaluating $ Evaluate.eval script
      logHeader $ "Result of applying " <> AST.getName name <> " on " <> T.pack (show args) <> ":"
      liftIO $ print res
  where
    logHeader s = logInfo ("\n" <> s) >> logInfo (T.replicate (T.length s) "-")
    liftError f = either (throwError . f) pure
    parseInput mInPath = do
      text <- liftIO $ getSourceCode mInPath
      liftError ErrorParsing $ Assemble.parseProgram (maybe "<stdin>" getInputFilePath mInPath) text
    assembleInput mInPath = do
      text <- liftIO $ getSourceCode mInPath
      liftError ErrorAssembling $ Assemble.assemble (maybe "<stdin>" getInputFilePath mInPath) text


getSourceCode :: MonadIO m => Maybe InputFilePath -> m Text
getSourceCode Nothing                     = pack <$> liftIO getContents
getSourceCode (Just (InputFilePath path)) = pack <$> liftIO (readFile path)


writeObjectCode :: MonadIO m => Maybe OutputFilePath -> ByteString -> m ()
writeObjectCode (Just (OutputFilePath path)) bs =
  liftIO $ writeFile path (unpack (encodeHex bs))
writeObjectCode Nothing bs =
  logInfo $ encodeHex bs


main :: IO ()
main = do
  cmd <- O.execParser commandInfo
  runExceptT (runCommand cmd) >>= \case
    Left err ->
      logError err
    Right () ->
      pure ()
