{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module PlutusCore.Assembler.EntryPoint (main) where


import           Data.Text                               (pack, unpack)
import qualified Options.Applicative                     as O
import           System.IO                               (FilePath, getContents,
                                                          print, putStrLn,
                                                          readFile, writeFile)
import           Text.Hex                                (encodeHex)

import qualified Data.Text                               as T
import qualified Plutus.V1.Ledger.Scripts                as Scripts
import qualified PlutusCore.Assembler.Assemble           as Assemble
import qualified PlutusCore.Assembler.Evaluate           as Evaluate
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Types.ErrorMessage (ErrorMessage (..))
import qualified PlutusCore.Pretty                       as Pretty


newtype InputFilePath = InputFilePath FilePath


newtype OutputFilePath = OutputFilePath FilePath


data Command
  = CommandAssemble
    (Maybe InputFilePath)
    (Maybe OutputFilePath)
  | CommandRun
    (Maybe InputFilePath)


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
      O.command "run" (O.info runP (O.progDesc "Run the Pluto code in Plutus evauator"))
    ]
  where
    assembleP =
      CommandAssemble
      <$> inputFilePath
      <*> outputFilePath
    runP =
      CommandRun
      <$> inputFilePath


commandInfo :: O.ParserInfo Command
commandInfo =
  O.info (command O.<**> O.helper)
   ( O.fullDesc
  <> O.progDesc "Pluto assembles into Plutus Core bytecode"
  <> O.header "pluto - Untyped Plutus Core assembler"
   )


runCommand :: Command -> IO ()
runCommand = \case
  CommandAssemble mInPath mOutPath -> do
      text <- getSourceCode mInPath
      case Assemble.assemble text of
        Left (ErrorMessage err) ->
          putStrLn $ "Error: " <> unpack err
        Right bs ->
          writeObjectCode mOutPath bs
  CommandRun mInPath -> do
      text <- getSourceCode mInPath
      case Assemble.translate text of
        Left (ErrorMessage err) ->
          putStrLn $ "Error(assembler): " <> unpack err
        Right bs -> do
          -- TODO: Depending on the need, enable/disable individiual dumps in CLI arguments.
          let putHeader s = putStrLn ("\n" <> s) >> putStrLn (T.unpack $ T.replicate (length s) "-")
          putHeader "UPLC"
          print $ Scripts.unScript bs
          putHeader "UPLC (pretty)"
          putStrLn $ Pretty.display $ Scripts.unScript bs
          case Evaluate.eval bs of
            Left err ->
              putStrLn $ "Error(eval): " <> show err
            Right (exBudget, traces) -> do
              putHeader "ExBudget"
              print exBudget
              putHeader "Script traces"
              forM_ traces $ \trace ->
                putStrLn $ unpack trace


getSourceCode :: Maybe InputFilePath -> IO Text
getSourceCode Nothing                     = pack <$> getContents
getSourceCode (Just (InputFilePath path)) = pack <$> readFile path


writeObjectCode :: Maybe OutputFilePath -> ByteString -> IO ()
writeObjectCode (Just (OutputFilePath path)) bs =
  writeFile path (unpack (encodeHex bs))
writeObjectCode Nothing bs =
  putStrLn (unpack (encodeHex bs))


main :: IO ()
main = O.execParser commandInfo >>= runCommand
