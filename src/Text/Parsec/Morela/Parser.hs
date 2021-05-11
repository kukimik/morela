{-# LANGUAGE OverloadedStrings #-}

module Text.Parsec.Morela.Parser (AST (..), document) where

import Control.Monad (void, when)
import Data.Char (isAlphaNum, isControl, isSpace)
import Data.Maybe
import Data.Text.Lazy
import Morela.Types
import Text.Parsec
import Text.Parsec.Text.Lazy

data AST
  = T {tName :: TableName, tComment :: Maybe Comment}
  | A {aName :: AttributeName, aTypeName :: Maybe TypeName, aIsPK :: Bool, aIsNN :: Bool, aComment :: Maybe Comment}
  | U {uAttributeNames :: [AttributeName]}
  | F {fReferencedTableName :: TableName, fAttributeNames1 :: [AttributeName], fAttributeNames2 :: [AttributeName]}
  | C {cSQLCondition :: SQLCondition}
  deriving (Show, Eq)

document :: Parser [AST]
document = do
  skipMany (morelaComment <|> blanks)
  catMaybes <$> manyTill top eof
  where
    top =
      (table <?> "table declaration")
        <|> (try attribute <?> "attribute")
        <|> (try uniqueConstraint <?> "unique constraint")
        <|> (try foreignKeyConstraint <?> "foreign key constraint")
        <|> (try checkConstraint <?> "check constraint")
        <|> (morelaComment <?> "comment")
        <|> blanks
    blanks = many1 (space <?> "whitespace") >> return Nothing

table :: Parser (Maybe AST)
table = do
  n <- between (char '[') (char ']') ident
  c <- option Nothing (Just <$> comment)
  eolMorelaComment
  return $ Just $ T {tName = n, tComment = c}

attribute :: Parser (Maybe AST)
attribute = do
  attrConstraints <- many $ oneOf "*! \t"
  let (ispk, isnn) = ('*' `elem` attrConstraints, '!' `elem` attrConstraints)
  n <- ident
  t <- option Nothing (Just <$> typeDeclaration)
  c <- option Nothing (Just <$> comment)
  eolMorelaComment
  return $
    Just $
      A {aName = n, aTypeName = t, aIsPK = ispk, aIsNN = isnn, aComment = c}

checkConstraint :: Parser (Maybe AST)
checkConstraint = do
  _ <- string "&CK"
  spacesNoNew
  c <- identQuoted
  eolMorelaComment
  return $
    Just $
      C {cSQLCondition = c}

uniqueConstraint :: Parser (Maybe AST)
uniqueConstraint = do
  _ <- string "&UQ"
  spacesNoNew
  as <- attributesList
  eolMorelaComment
  return $
    Just $
      U {uAttributeNames = as}

foreignKeyConstraint :: Parser (Maybe AST)
foreignKeyConstraint = do
  _ <- string "&FK"
  spacesNoNew
  a1s <- attributesList
  spacesNoNew
  _ <- string "->"
  spacesNoNew
  t <- ident
  _ <- char '.'
  a2s <- attributesList
  eolMorelaComment
  return $
    Just $
      F {fReferencedTableName = t, fAttributeNames1 = a1s, fAttributeNames2 = a2s}

attributesList :: Parser [AttributeName]
attributesList = sepBy1 ident (char ',')

morelaComment :: Parser (Maybe AST)
morelaComment = do
  _ <- char '#'
  _ <- manyTill anyChar $ try eol
  return Nothing

comment :: Parser Text
comment = do
  _ <- char '^'
  identQuoted

ident :: Parser Text
ident = do
  spacesNoNew
  n <- identQuoted <|> identNoSpace
  spacesNoNew
  return n

identQuoted :: Parser Text
identQuoted = do
  quote <- oneOf "'\"`"
  let p =
        satisfy (\c -> c /= quote && not (isControl c))
          <?> "any character except " <> [quote] <> " or control characters"
  n <- fmap pack (many1 p)
  _ <- char quote
  return n

identNoSpace :: Parser Text
identNoSpace = do
  let p =
        satisfy (\c -> c == '_' || isAlphaNum c)
          <?> "letter, digit or underscore"
  fmap pack (many1 p)

typeDeclaration :: Parser Text
typeDeclaration = do
  spacesNoNew
  let p =
        satisfy (\c -> c `elem` ['_',',','(',')'] || isAlphaNum c)
          <?> "letter, digit, underscore, comma or bracket"
  t <- fmap pack (many1 p)
  spacesNoNew
  return t

eolMorelaComment :: Parser ()
eolMorelaComment = spacesNoNew >> (eol <|> void comment)

spacesNoNew :: Parser ()
spacesNoNew = skipMany $ satisfy $ \c -> c /= '\n' && c /= '\r' && isSpace c

eol :: Parser ()
eol =
  eof <|> do
    c <- oneOf "\n\r"
    when (c == '\r') $ optional $ char '\n'
