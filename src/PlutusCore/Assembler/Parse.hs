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
term0 = lambdaTerm <|> term1


lambdaTerm :: Parser Term
lambdaTerm = todo


term1 :: Parser Term
term1 = do
  t  <- term1
  ts <- P.many term2
  return $ foldl Apply t ts


term2 :: Parser Term
term2 = ifTerm <|> letTerm <|> term3


ifTerm :: Parser Term
ifTerm = todo


letTerm :: Parser Term
letTerm = todo


term3 :: Parser Term
term3 = forceTerm <|> delayTerm <|> infixApplyTerm <|> term4


forceTerm :: Parser Term
forceTerm = todo


delayTerm :: Parser Term
delayTerm = todo


infixApplyTerm :: Parser Term
infixApplyTerm = todo


term4 :: Parser Term
term4 = constantTerm <|> builtinTerm <|> errorTerm <|> parenthesizedTerm


constantTerm :: Parser Term
constantTerm = todo


builtinTerm :: Parser Term
builtinTerm = todo


errorTerm :: Parser Term
errorTerm = todo


parenthesizedTerm :: Parser Term
parenthesizedTerm = todo


todo :: a
todo = todo
