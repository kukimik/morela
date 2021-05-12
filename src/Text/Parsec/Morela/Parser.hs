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
  | I {iAttributeNames :: [AttributeName], iIsUnique :: Bool}
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
        <|> (try regularIndex <?> "index")
        <|> (try uniqueIndex <?> "unique index")
        <|> (comment <?> "comment")
        <|> blanks
    blanks = many1 (space <?> "whitespace") >> return Nothing

table :: Parser (Maybe AST)
table = do
  n <- between (char '[') (char ']') ident
  c <- option Nothing (Just <$> dbObjectComment)
  eolComment
  return $ Just $ T {tName = n, tComment = c}

attribute :: Parser (Maybe AST)
attribute = do
  attrConstraints <- many $ oneOf "*! \t"
  let (ispk, isnn) = ('*' `elem` attrConstraints, '!' `elem` attrConstraints)
  n <- ident
  t <- option Nothing (Just <$> typeDeclaration)
  c <- option Nothing (Just <$> dbObjectComment)
  eolComment
  return $
    Just $
      A {aName = n, aTypeName = t, aIsPK = ispk, aIsNN = isnn, aComment = c}

checkConstraint :: Parser (Maybe AST)
checkConstraint = do
  _ <- string "&CK"
  spacesNoNew
  c <- identQuoted
  eolComment
  return $
    Just $
      C {cSQLCondition = c}

uniqueConstraint :: Parser (Maybe AST)
uniqueConstraint = do
  as <- helperUniquesAndIndexes "&UQ"
  return $
    Just $
      U {uAttributeNames = as}

regularIndex :: Parser (Maybe AST)
regularIndex = do
  as <- helperUniquesAndIndexes "&IX"
  return $
    Just $
      I {iAttributeNames = as, iIsUnique = False}

uniqueIndex :: Parser (Maybe AST)
uniqueIndex = do
  as <- helperUniquesAndIndexes "&UX"
  return $
    Just $
      I {iAttributeNames = as, iIsUnique = True}

helperUniquesAndIndexes :: String -> Parser[AttributeName]
helperUniquesAndIndexes cmd = do
  _ <- string cmd
  spacesNoNew
  as <- attributesList
  eolComment
  return as

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

dbObjectComment :: Parser Text
dbObjectComment = do
  spacesNoNew
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
