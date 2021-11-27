{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.EntryPoint (main) where


import Data.Text (pack, unpack)
import System.IO (FilePath, putStrLn, getContents, readFile, writeFile)
import Text.Hex (encodeHex)
import qualified Options.Applicative as O

import PlutusCore.Assembler.Prelude
import PlutusCore.Assembler.Assemble (assemble)
import PlutusCore.Assembler.Types.ErrorMessage (ErrorMessage (..))


newtype InputFilePath = InputFilePath FilePath


newtype OutputFilePath = OutputFilePath FilePath


data Command =
  Command
  (Maybe InputFilePath)
  (Maybe OutputFilePath)


inputFilePath :: O.Parser (Maybe InputFilePath)
inputFilePath =
  O.argument (Just . InputFilePath <$> O.str) (O.metavar "INPUT" <> O.value Nothing <> O.help "The input file path: defaults to stdin")


outputFilePath :: O.Parser (Maybe OutputFilePath)
outputFilePath =
  O.argument (Just . OutputFilePath <$> O.str) (O.metavar "OUTPUT" <> O.value Nothing <> O.help "The output file path: defaults to stdout")


command :: O.Parser Command
command =
  Command
  <$> inputFilePath
  <*> outputFilePath


commandInfo :: O.ParserInfo Command
commandInfo =
  O.info (command O.<**> O.helper)
   ( O.fullDesc
  <> O.progDesc "Pluto assembles into Plutus Core bytecode"
  <> O.header "pluto - Untyped Plutus Core assembler"
   )


runCommand :: Command -> IO ()
runCommand (Command mInPath mOutPath) = do
  txt <- getSourceCode mInPath
  case assemble txt of
    Left (ErrorMessage err) ->
      putStrLn $ "Error: " <> unpack err
    Right bs ->
      writeObjectCode mOutPath bs


getSourceCode :: Maybe InputFilePath -> IO Text
getSourceCode Nothing = pack <$> getContents
getSourceCode (Just (InputFilePath path)) = pack <$> readFile path


writeObjectCode :: Maybe OutputFilePath -> ByteString -> IO ()
writeObjectCode (Just (OutputFilePath path)) bs =
  writeFile path (unpack (encodeHex bs))
writeObjectCode Nothing bs =
  putStrLn (unpack (encodeHex bs))


main :: IO ()
main = O.execParser commandInfo >>= runCommand
