{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Logging and error handling
module PlutusCore.Assembler.App
  ( Error(..)
  , logInfo
  , logShower
  , logError
  , logHeader
  , liftError
  )
where

import           System.IO                               (hPutStrLn, putStrLn,
                                                          stderr)

import           Control.Exception                       (Exception)
import qualified Data.Text                               as T
import qualified Plutus.V1.Ledger.Scripts                as Scripts
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Types.ErrorMessage (ErrorMessage (..))
import qualified Shower



-- | An error when running the CLI command.
--
-- Gathers all possible errors in the application.
data Error
  = ErrorParsing ErrorMessage
  | ErrorAssembling ErrorMessage
  | ErrorEvaluating Scripts.ScriptError
  | ErrorOther Text
  deriving (Eq, Show, Exception)

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

logHeader :: MonadIO m => Text -> m ()
logHeader s = logInfo ("\n" <> s) >> logInfo (T.replicate (T.length s) "-")

liftError :: MonadError e m => (a -> e) -> Either a b -> m b
liftError f = either (throwError . f) pure

