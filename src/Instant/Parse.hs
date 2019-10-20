module Instant.Parse where

import           Control.Applicative        (liftA2)
import           Control.Monad
import           Control.Monad.Identity
import           Data.Void
import           Data.List as DL
import           Data.Bifunctor
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Prelude hiding (lex)

import Instant.Types.Syntax


type Parser = ParsecT Void String Identity
parseInstant :: String -> String -> Either String (AST 'P)
parseInstant file inp = first
  (concat . fmap parseErrorPretty . bundleErrors)
  (parse ast file inp)


skip :: Parser ()
skip = L.space (void spaceChar) empty empty


lex :: Parser a -> Parser a
lex = L.lexeme skip

-- |Identifier starting with lower-case character
lId :: Parser String
lId = lex $ liftA2 (:) lowerChar (many alphaNumChar)


unsigned :: Parser Int
unsigned = lex $ L.decimal


-- |Specific operator
operator :: String -> Parser ()
operator o =
  lex $ try $ string o *> notFollowedBy (oneOf "=+-/*;")


-- |Surround parser with parentheses
paren :: Parser a -> Parser a
paren = between (L.symbol skip "(") (L.symbol skip ")")


astE1 :: Parser (AST 'E1)
astE1 = msum
  [ ASTInt <$> unsigned
  , ASTVar <$> lId
  , ASTParen <$> paren astE3
  ]


astE2 :: Parser (AST 'E2)
astE2 = msum
  [ liftA2 ASTMult (try $ astE1 <* operator "*") astE2
  , liftA2 ASTDiv (try $ astE1 <* operator "/") astE2
  , AST12 <$> astE1
  ]

astE3 :: Parser (AST 'E3)
astE3 = msum
  [ liftA2 ASTPlus (try $ astE2 <* operator "+") astE3
  , liftA2 ASTMinus (try $ astE2 <* operator "-") astE3
  , AST23 <$> astE2
  ]

astSt :: Parser (AST 'St)
astSt = msum
  [ liftA2 ASTAss (try $ lId <* operator "=") astE3
  , ASTExpr <$> astE3
  ]

astP :: Parser (AST 'P)
astP = AST <$> sepBy astSt (operator ";")


ast :: Parser (AST 'P)
ast = (skip *> astP <* eof)
