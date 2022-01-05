{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}


module PlutusCore.Assembler.EntryPoint (main)  where


import           Data.ByteString                         (hPut, writeFile)
import           Data.Text                               (pack)
import qualified Options.Applicative                     as O
import           System.Exit                             (ExitCode (ExitFailure),
                                                          exitWith)
import           System.IO                               (FilePath, getContents,
                                                          print, stdout)

import qualified Data.Bifunctor                          as Bifunctor
import           Data.List.NonEmpty                      (nonEmpty)
import qualified Data.Text                               as T
import qualified Plutus.V1.Ledger.Scripts                as Scripts
import           PlutusCore.Assembler.App
import qualified PlutusCore.Assembler.Assemble           as Assemble
import qualified PlutusCore.Assembler.Evaluate           as Evaluate
import           PlutusCore.Assembler.Prelude
import qualified PlutusCore.Assembler.Types.AST          as AST
import           PlutusCore.Assembler.Types.ErrorMessage (ErrorMessage (..))
import qualified PlutusCore.Data                         as PLC
import qualified PlutusCore.Pretty                       as Pretty


newtype InputFilePath = InputFilePath { getInputFilePath :: FilePath }

newtype OutputFilePath = OutputFilePath { _getOutputFilePath :: FilePath }

newtype Verbose = Verbose Bool

type Shrinking = Bool

data Command
  = CommandAssemble
    Shrinking
    (Maybe InputFilePath)
    (Maybe OutputFilePath)
  | CommandRun
    Shrinking
    (Maybe InputFilePath)
    [PLC.Data]
  | CommandEval
    Shrinking
    (Maybe InputFilePath)
    AST.Name
    [AST.Term ()]


inputFilePath :: O.Parser (Maybe InputFilePath)
inputFilePath =
  O.argument (Just . InputFilePath <$> O.str) (O.metavar "INPUT" <> O.value Nothing <> O.help "The input file path: defaults to stdin")


outputFilePath :: O.Parser (Maybe OutputFilePath)
outputFilePath =
  O.argument (Just . OutputFilePath <$> O.str) (O.metavar "OUTPUT" <> O.value Nothing <> O.help "The output file path: defaults to stdout")

shrinking :: O.Parser Bool
shrinking = O.switch (O.long "shrinking" <> O.short 's' <> O.help "Shrink the uplc after assembly")

command :: O.Parser (Command, Verbose)
command = do
  cmd <- O.subparser . mconcat $
    [ O.command "assemble" (O.info assembleP (O.progDesc "Assemble to Plutus bytecode, and display the HEX")),
      O.command "run" (O.info runP (O.progDesc "Run the Pluto code in Plutus evauator")),
      O.command "eval" (O.info runBindingP (O.progDesc "Evaluate a let binding in the program"))
    ]
  verbose <- Verbose <$> O.switch (O.long "verbose" <> O.short 'v' <> O.help "Dump ASTs along the way")
  _ <- shrinking 
  pure (cmd, verbose)
  where
    assembleP =
      CommandAssemble
      <$> shrinking
      <*> inputFilePath
      <*> outputFilePath
    runP =
      CommandRun
      <$> shrinking
      <*> inputFilePath
      <*> O.many (O.argument dataReader (O.metavar "PLC.Data encoded"))
    runBindingP =
      CommandEval
      <$> shrinking
      <*> inputFilePath
      <*> fmap AST.Name (O.strArgument (O.metavar "BINDING" <> O.help "Name of the variable bound in the let block"))
      <*> O.many (O.argument termReader (O.metavar "ARG"))
    termReader :: O.ReadM (AST.Term ())
    termReader =
      O.eitherReader $ \(T.pack -> s) ->
        bimap (T.unpack . getErrorMessage) (void . AST.unProgram) $ Assemble.parseProgram "<cli-arg>" s
    dataReader :: O.ReadM PLC.Data
    dataReader =
      O.eitherReader $ \(Assemble.parsePlutusData "<cli-arg>" . T.pack -> x) ->
        Bifunctor.first show x


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
    CommandAssemble shrink mInPath mOutPath -> do
      bin <- assembleInput shrink mInPath
      writeObjectCode mOutPath bin
    CommandRun shrink mInPath args -> do
      ast <- parseInput mInPath
      when verbose $ do
        logHeader "AST"
        logShower $ void ast
      prog <- liftError ErrorAssembling $ 
        (if shrink 
            then Assemble.translateAndShrink 
            else Assemble.translate
        ) ast
      when verbose $ do
        logHeader "UPLC"
        logShower $ Scripts.unScript prog
        logHeader "UPLC (pretty)"
        logInfo $ Pretty.display $ Scripts.unScript prog
      evalRes <- liftError ErrorEvaluating $
        Evaluate.evalWithArgs args prog
      logEvalResult evalRes
    CommandEval shrink mInPath name args -> do
      evalRes <-
        either throwError pure . (if shrink then Evaluate.shrinkEvalToplevelBinding else Evaluate.evalToplevelBinding) name args . void
          =<< parseInput mInPath
      logEvalResult evalRes
  where
    parseInput mInPath = do
      text <- liftIO $ getSourceCode mInPath
      liftError ErrorParsing $ Assemble.parseProgram (maybe "<stdin>" getInputFilePath mInPath) text
    assembleInput shrink mInPath = do
      text <- liftIO $ getSourceCode mInPath
      liftError ErrorAssembling $ (if shrink then Assemble.assembleAndShrink else Assemble.assemble) (maybe "<stdin>" getInputFilePath mInPath) text
    logEvalResult (exBudget, traces, res) = do
      logHeader "ExBudget"
      liftIO $ print exBudget
      case nonEmpty traces of
        Nothing -> pure ()
        Just traces' -> do
          logHeader "Traces"
          forM_ traces' $ \trace -> do
            logInfo trace
      logHeader "Result"
      liftIO $ print res


getSourceCode :: MonadIO m => Maybe InputFilePath -> m Text
getSourceCode Nothing                     = pack <$> liftIO getContents
getSourceCode (Just (InputFilePath path)) = pack <$> liftIO (readFile path)


writeObjectCode :: MonadIO m => Maybe OutputFilePath -> ByteString -> m ()
writeObjectCode (Just (OutputFilePath path)) bs =
  liftIO $ writeFile path bs
writeObjectCode Nothing bs =
  liftIO $ hPut stdout bs


main :: IO ()
main = do
  (cmd, verbose) <- O.execParser commandInfo
  runExceptT (runCommand cmd verbose) >>= \case
    Left err -> do
      logError err
      exitWith (ExitFailure 1)
    Right () ->
      pure ()
