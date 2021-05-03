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
  = T {tName :: TableName}
  | A {aName :: AttributeName, aTypeName :: Maybe TypeName, aIsPK :: Bool, aIsNN :: Bool}
  | U {uAttributeNames :: [AttributeName]}
  | F {fReferencedTableName :: TableName, fAttributeNames1 :: [AttributeName], fAttributeNames2 :: [AttributeName]}
  | C {cSQLCondition :: SQLCondition}
  deriving (Show, Eq)

document :: Parser [AST]
document = do
  skipMany (comment <|> blanks)
  catMaybes <$> manyTill top eof
  where
    top =
      (table <?> "table declaration")
        <|> (try attribute <?> "attribute")
        <|> (try uniqueConstraint <?> "unique constraint")
        <|> (try foreignKeyConstraint <?> "foreign key constraint")
        <|> (try checkConstraint <?> "check constraint")
        <|> (comment <?> "comment")
        <|> blanks
    blanks = many1 (space <?> "whitespace") >> return Nothing

table :: Parser (Maybe AST)
table = do
  n <- between (char '[') (char ']') ident
  eolComment
  return $ Just $ T {tName = n}

attribute :: Parser (Maybe AST)
attribute = do
  attrConstraints <- many $ oneOf "*! \t"
  let (ispk, isnn) = ('*' `elem` attrConstraints, '!' `elem` attrConstraints)
  n <- ident
  t <- option Nothing (Just <$> ident)
  eolComment
  return $
    Just $
      A {aName = n, aTypeName = t, aIsPK = ispk, aIsNN = isnn}

checkConstraint :: Parser (Maybe AST)
checkConstraint = do
  string "&CK"
  spacesNoNew
  c <- identQuoted
  eolComment
  return $
    Just $
      C {cSQLCondition = c}

uniqueConstraint :: Parser (Maybe AST)
uniqueConstraint = do
  string "&UQ"
  spacesNoNew
  as <- attributesList
  eolComment
  return $
    Just $
      U {uAttributeNames = as}

foreignKeyConstraint :: Parser (Maybe AST)
foreignKeyConstraint = do
  string "&FK"
  spacesNoNew
  a1s <- attributesList
  spacesNoNew
  string "->"
  spacesNoNew
  t <- ident
  char '.'
  a2s <- attributesList
  eolComment
  return $
    Just $
      F {fReferencedTableName = t, fAttributeNames1 = a1s, fAttributeNames2 = a2s}

attributesList :: Parser [AttributeName]
attributesList = sepBy1 ident (char ',')

comment :: Parser (Maybe AST)
comment = do
  _ <- char '#'
  _ <- manyTill anyChar $ try eol
  return Nothing

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
          <?> "any character except " ++ [quote] ++ " or control characters"
  n <- fmap pack (many1 p)
  _ <- char quote
  return n

identNoSpace :: Parser Text
identNoSpace = do
  let p =
        satisfy (\c -> c == '_' || isAlphaNum c)
          <?> "letter, digit or underscore"
  fmap pack (many1 p)

eolComment :: Parser ()
eolComment = spacesNoNew >> (eol <|> void comment)

spacesNoNew :: Parser ()
spacesNoNew = skipMany $ satisfy $ \c -> c /= '\n' && c /= '\r' && isSpace c

eol :: Parser ()
eol =
  eof <|> do
    c <- oneOf "\n\r"
    when (c == '\r') $ optional $ char '\n'
