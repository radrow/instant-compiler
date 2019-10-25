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

import Instant.Syntax


type Parser = ParsecT Void String Identity
parseInstant :: String -> String -> Either String (AST 'P)
parseInstant filename inp = first
  (concat . fmap parseErrorPretty . bundleErrors)
  (parse ast filename inp)


getAnn :: Parser Ann
getAnn = do
  p <- getSourcePos
  return $ Ann
    (fromIntegral $ unPos $ sourceLine p)
    (fromIntegral $ unPos $ sourceColumn p)
    (sourceName p)


withAnn :: Parser (Ann -> a) -> Parser a
withAnn p = liftA2 (flip ($)) getAnn p


skip :: Parser ()
skip = L.space (void spaceChar) empty empty


lex :: Parser a -> Parser a
lex = L.lexeme skip


lId :: Parser String
lId = lex $ liftA2 (:) lowerChar (many alphaNumChar)


unsigned :: Parser Int
unsigned = lex $ L.decimal


operator :: String -> Parser ()
operator o =
  lex $ try $ string o *> notFollowedBy (oneOf "=+-/*;")


paren :: Parser a -> Parser a
paren = between (L.symbol skip "(") (L.symbol skip ")")


infixL :: Parser (Ann -> a -> b -> a) -> Parser b -> a -> Parser a
infixL op p x = do
  f <- withAnn op
  y <- p
  let r = f x y
  infixL op p r <|> return r


astE1 :: Parser (AST 'E1)
astE1 = choice
  [ withAnn (pure ASTPlus) <*> (try $ astE2 <* operator "+") <*> astE1
  , AST21 <$> astE2
  ]


astE2 :: Parser (AST 'E2)
astE2 = choice
  [ try $ (AST32 <$> astE3) >>= infixL (ASTMinus <$ operator "-") astE3
  , AST32 <$> astE3
  ]


astE3 :: Parser (AST 'E3)
astE3 = choice
  [ try $ (AST43 <$> astE4) >>=
    infixL ( ASTMult <$ operator "*" <|>
             ASTDiv <$ operator "/"
           ) astE4
  , AST43 <$> astE4
  ]


astE4 :: Parser (AST 'E4)
astE4 = choice
  [ withAnn (pure ASTInt) <*> unsigned
  , withAnn (pure ASTVar) <*> lId
  , ASTParen <$> paren astE1
  ]

astSt :: Parser (AST 'St)
astSt = choice
  [ withAnn (pure ASTAss) <*> (try $ lId <* operator "=") <*> astE1
  , withAnn (pure ASTExpr) <*> astE1
  ]


astP :: Parser (AST 'P)
astP = AST <$> sepBy astSt (operator ";")


ast :: Parser (AST 'P)
ast = (skip *> astP <* eof)
