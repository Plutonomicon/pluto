{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}


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
import           PlutusCore.Assembler.Types.ErrorMessage (ErrorMessage (..))
import qualified PlutusCore.Pretty                       as Pretty
import qualified Shower


newtype InputFilePath = InputFilePath FilePath


newtype OutputFilePath = OutputFilePath FilePath


data Command
  = CommandAssemble
    (Maybe InputFilePath)
    (Maybe OutputFilePath)
  | CommandRun
    (Maybe InputFilePath)
  | CommandRunBinding
    (Maybe InputFilePath)
    Text
    [Text]


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
      O.command "runbinding" (O.info runBindingP (O.progDesc "Evaluate a let binding in the program"))
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
      CommandRunBinding
      <$> inputFilePath
      <*> O.strArgument (O.metavar "BINDING" <> O.help "Name of the variable bound in the let block")
      <*> O.many (O.strArgument $ O.metavar "ARG")


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
  ErrorParsing (ErrorMessage em)    -> f "parser" em
  ErrorAssembling (ErrorMessage em) -> f "assembler" em
  ErrorEvaluating em                -> f "eval" (T.pack $ show em)
  where
    f name msg =
      liftIO $ hPutStrLn stderr $ "Error(" <> name <> "): " <> T.unpack msg


runCommand :: forall m. (MonadIO m, MonadError Error m) => Command -> m ()
runCommand cmd = do
  case cmd of
    CommandAssemble mInPath mOutPath -> do
      text <- getSourceCode mInPath
      prog <- liftError ErrorAssembling $ Assemble.assemble text
      writeObjectCode mOutPath prog
    CommandRun mInPath -> do
      text <- liftIO $ getSourceCode mInPath
      ast <- liftError ErrorParsing $ Assemble.parseProgram text
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
    CommandRunBinding mInPath fnName fnArgs -> do
      text <- getSourceCode mInPath
      ast <- liftError ErrorParsing $ Assemble.parseProgram text
      logShower $ void ast
      prog <- liftError ErrorAssembling $ Assemble.translate ast
      (_, _, res) <- liftError ErrorEvaluating $ Evaluate.eval prog
      liftIO $ print res
      logInfo $ "TODO: Unsupported " <> fnName <> T.pack (show fnArgs)
  where
    logHeader s = logInfo ("\n" <> s) >> logInfo (T.replicate (T.length s) "-")
    liftError f = either (throwError . f) pure


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
