{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Morela.Parse
  ( toDiagram
  )
where

import           Morela.Types

import           Control.Monad          (when)
import           Data.Functor ((<&>))
import           Data.List              (find)
import           Data.Maybe
import           Data.Text.Lazy         hiding (find, map, reverse)
import           Data.Text.Lazy.IO
import           System.IO              (Handle)
import           Text.Parsec
import           Text.Parsec.Morela.Parser (AST (..), GlobalOptions (..), document)
import           Text.Printf            (printf)

toDiagram :: [AST] -> Either String Diagram
toDiagram xs = do
  diag <- foldM step xs (emptyDiagram,Nothing) <$> (curry addMaybeTable)
  checkConstraints diag
  return diag
  where
    addMaybeTable :: Diagram -> Maybe Table -> Diagram
    addMaybeTable d Nothing = d
    addMaybeTable d@Diagram{..} (Just t) = d{ diagramTables = insert t diagramTables } -- TODO: fail on duplicate table name

    checkConstraints :: Diagram -> Either String ()
    checkConstraints = return () -- TODO: check whether constraints are valid

    step :: (Diagram, Maybe Table) -> AST -> Either String (Diagram, Maybe Table)
    step (d, Nothing) T{..} = Right (d, emptyTable tName)
    step (_, Nothing) _ = Left "Attribute or constraint comes before first table."
    step (d, t@(Just Table{})) T{..} =
      Right (
             (addMaybeTable d t)
            ,emptyTable tName
            )
    step (d, t@(Just Table{})) A{..} = -- TODO: the list appending below is evil, fix this
      Right (d,t{
               tableAttributes = tableAttributes ++ [Attribute {attributeName = aName, attributeType = aTypeName, attributeComment = Nothing, attributeStyleName = Nothing }]
              ,tableNNs = if aIsNN then tableNNs++[NNConstraint{nnAttributeName = aName}] else tableNNs
              ,tablePK = if aIsPK
                           then if tablePK = Just pk@PKConstraint{..}
                                  then pk{pkAttributeNames = pkAttributeNames++[aName]}
                                  else Just PKConstraint{pkAttributeNames = [aName]}
                           else tablePK -- TODO: make this readable
              })
    step (d, t@(Just Table{})) U{..} =
      Right (d,t{
               tableUQs = tableUQs ++ [UQConstraint{uqAttributeNames = uAttributeNames, uqStyleName = Nothing, uqComment = Nothing }]
              })
    step (d, t@(Just Table{})) F{..} =
      Right (d,t{
               tableFKs = tableFKs ++ [CKConstraint{fkReferencedTableName = fReferencedTableName, fkAttributeMapping = zip fAttributeNames1 fAttributeNames2 {- TODO: fail when lengths do not match! -}, fkStyleName = Nothing, fkComment = Nothing }]
              })
    step (d, t@(Just Table{})) C{..} =
      Right (d,t{
               tableCKs = tableCKs ++ [CKConstraint{ckSQLCondition = cSQLCondition, ckStyleName = Nothing, ckComment = Nothing }]
              })
