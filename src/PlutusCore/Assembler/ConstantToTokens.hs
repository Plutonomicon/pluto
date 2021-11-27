{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.ConstantToTokens ( constantToTokens ) where


import           Data.List                        (intercalate)
import qualified PlutusCore.Data                  as Data

import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Types.AST   (Constant (..), Data)
import qualified PlutusCore.Assembler.Types.Token as T


-- Every Constant has a unique constant representation as tokens,
-- which is what this function outputs.
constantToTokens :: Constant ann -> [T.Token]
constantToTokens =
  \case
    I _ x     -> [T.Integer x]
    S _ x     -> [T.ByteString x]
    T _ x     -> [T.Text x]
    U _       -> [T.OpenParen, T.CloseParen]
    B _ x     -> [T.Bool x]
    D _ d     -> [T.Data] <> dataToTokens d


dataToTokens :: Data -> [T.Token]
dataToTokens =
  \case
    Data.Constr i xs ->
      [T.Sigma, T.Integer i, T.Period, T.OpenBracket]
      <> intercalate [T.Comma] (dataToTokens <$> xs)
      <> [T.CloseBracket]
    Data.Map xs ->
      [T.OpenBrace]
      <> intercalate [T.Comma] (dataMapEntryToTokens <$> xs)
      <> [T.CloseBrace]
    Data.List xs ->
      [T.OpenBracket]
      <> intercalate [T.Comma] (dataToTokens <$> xs)
      <> [T.CloseBracket]
    Data.I x -> [T.Integer x]
    Data.B x -> [T.ByteString x]


dataMapEntryToTokens :: (Data, Data) -> [T.Token]
dataMapEntryToTokens (k, v) =
  dataToTokens k <> [T.Equals] <> dataToTokens v
