{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Parse ( parse ) where


import Data.Either.Extra (mapLeft)
import Data.Text (pack)
import qualified Text.Parsec.Prim as P

import PlutusCore.Assembler.Prelude
import PlutusCore.Assembler.Types.ErrorMessage (ErrorMessage (..))
import PlutusCore.Assembler.Types.AST (Program (..), Term (..))
import PlutusCore.Assembler.Types.Token (Token (..))


type Parser = P.Parsec [Token] ()


parse :: [Token] -> Either ErrorMessage Program
parse = mapLeft (ErrorMessage . pack . show) . P.parse program "input"


program :: Parser Program
program = Program <$> term


term :: Parser Term
term = term0


term0 :: Parser Term
term0 = do
  t  <- term1
  ts <- P.many term1
  return $ foldl Apply t ts


term1 :: Parser Term
term1 = todo


todo :: a
todo = todo
