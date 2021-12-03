{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}


module PlutusCore.Assembler.EntryPoint (main) where


import           Data.Text                               (pack, unpack)
import qualified Options.Applicative                     as O
import           System.IO                               (FilePath, getContents,
                                                          print, writeFile)
import           Text.Hex                                (encodeHex)

import qualified Data.Text                               as T
import qualified Plutus.V1.Ledger.Scripts                as Scripts
import           PlutusCore.Assembler.App
import qualified PlutusCore.Assembler.Assemble           as Assemble
import qualified PlutusCore.Assembler.Evaluate           as Evaluate
import           PlutusCore.Assembler.Prelude
import qualified PlutusCore.Assembler.Types.AST          as AST
import           PlutusCore.Assembler.Types.ErrorMessage (ErrorMessage (..))
import qualified PlutusCore.Pretty                       as Pretty


newtype InputFilePath = InputFilePath { getInputFilePath :: FilePath }

newtype OutputFilePath = OutputFilePath { _getOutputFilePath :: FilePath }

newtype Verbose = Verbose Bool

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


command :: O.Parser (Command, Verbose)
command = do
  cmd <- O.subparser . mconcat $
    [ O.command "assemble" (O.info assembleP (O.progDesc "Assemble to Plutus bytecode, and display the HEX")),
      O.command "run" (O.info runP (O.progDesc "Run the Pluto code in Plutus evauator")),
      O.command "eval" (O.info runBindingP (O.progDesc "Evaluate a let binding in the program"))
    ]
  verbose <- Verbose <$> O.switch (O.long "verbose" <> O.short 'v' <> O.help "Dump ASTs along the way")
  pure (cmd, verbose)
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


commandInfo :: O.ParserInfo (Command, Verbose)
commandInfo =
  O.info (command O.<**> O.helper)
   ( O.fullDesc
  <> O.progDesc "Pluto assembles into Plutus Core bytecode"
  <> O.header "pluto - Untyped Plutus Core assembler"
   )

runCommand :: forall m. (MonadIO m, MonadError Error m) => Command -> Verbose -> m ()
runCommand cmd (Verbose verbose) = do
  case cmd of
    CommandAssemble mInPath mOutPath -> do
      bin <- assembleInput mInPath
      writeObjectCode mOutPath bin
    CommandRun mInPath -> do
      ast <- parseInput mInPath
      when verbose $ do
        logHeader "AST"
        logShower $ void ast
      prog <- liftError ErrorAssembling $ Assemble.translate ast
      when verbose $ do
        logHeader "UPLC"
        logShower $ Scripts.unScript prog
        logHeader "UPLC (pretty)"
        logInfo $ Pretty.display $ Scripts.unScript prog
      (exBudget, traces, res) <- liftError ErrorEvaluating $ Evaluate.eval prog
      when verbose $ do
        logHeader "ExBudget"
        liftIO $ print exBudget
        logHeader "Script traces"
        forM_ traces $ \trace ->
          logInfo trace
        logHeader "Script result"
      liftIO $ print res
    CommandEval mInPath name args -> do
      res <-
        either throwError pure . Evaluate.evalToplevelBinding name args . void
          =<< parseInput mInPath
      liftIO $ print res
  where
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
  (cmd, verbose) <- O.execParser commandInfo
  runExceptT (runCommand cmd verbose) >>= \case
    Left err ->
      logError err
    Right () ->
      pure ()
