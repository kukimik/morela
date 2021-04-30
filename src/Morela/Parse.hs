{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Morela.Parse
  ( toDiagram
  )
where

import           Morela.Types

import Control.Monad(foldM)
import           Data.Maybe
import           Data.Text.Lazy         hiding (zip)
import           Text.Parsec.Morela.Parser (AST (..))
import qualified Data.Set as Set

toDiagram :: [AST] -> Either String Diagram
toDiagram xs = do
  diag <- (uncurry addMaybeTable) <$> foldM step (emptyDiagram,Nothing) xs
  checkConstraints diag
  return diag
  where
    addMaybeTable :: Diagram -> Maybe Table -> Diagram
    addMaybeTable d Nothing = d
    addMaybeTable d@Diagram{..} (Just t) = d{ diagramTables = Set.insert t diagramTables } -- TODO: fail on duplicate table name
    checkConstraints :: Diagram -> Either String ()
    checkConstraints = undefined -- TODO: check whether constraints are valid

    step :: (Diagram, Maybe Table) -> AST -> Either String (Diagram, Maybe Table)
    step (d, Nothing) T{..} = Right (d, Just $ emptyTable tName)
    step (_, Nothing) _ = Left "Attribute or constraint comes before first table."
    step (d, t@(Just Table{})) T{..} =
      Right (
             addMaybeTable d t
            ,Just $ emptyTable tName
            )
    step (d, Just t@Table{}) A{..} = -- TODO: the list appending below is evil, fix this
      Right (d, Just t{
               tableAttributes = (tableAttributes t) <> [Attribute {attributeName = aName, attributeType = aTypeName, attributeComment = Nothing, attributeStyleName = Nothing }]
              ,tableNNs = if aIsNN then tableNNs t <> [NNConstraint{nnAttributeName = aName}] else tableNNs t
              ,tablePK = if aIsPK then (tablePK t){pkAttributeNames = pkAttributeNames (tablePK t) <> [aName]} else tablePK t -- TODO: make this readable
              })
    step (d, (Just t@Table{})) U{..} =
      Right (d, Just t{
               tableUQs = (tableUQs t) <> [UQConstraint{uqAttributeNames = uAttributeNames, uqStyleName = Nothing, uqComment = Nothing }]
              })
    step (d, (Just t@Table{})) F{..} =
      Right (d, Just t{
               tableFKs = (tableFKs t) <> [FKConstraint{fkReferencedTableName = fReferencedTableName, fkAttributeMapping = zip fAttributeNames1 fAttributeNames2 {- TODO: fail when lengths do not match! -}, fkStyleName = Nothing, fkComment = Nothing }]
              })
    step (d, (Just t@Table{})) C{..} =
      Right (d,Just t{
               tableCKs = (tableCKs t) <> [CKConstraint{ckSQLCondition = cSQLCondition, ckStyleName = Nothing, ckComment = Nothing }]
              })
