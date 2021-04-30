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
  diag <- foldM step xs (emptyDiagram,Nothing) <$> (uncurry addMaybeTable)
  checkConstraints diag
  return diag
  where
    addMaybeTable :: Diagram -> Maybe Table -> Diagram
    addMaybeTable d Nothing = d
    addMaybeTable d@Diagram{..} (Just t) = d{ diagramTables = Set.insert t diagramTables } -- TODO: fail on duplicate table name

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
              ,tablePK = if aIsPK then (tablePK t){pkAttributeNames = pkAttributeNames++[aName]} else tablePK -- TODO: make this readable
              })
    step (d, t@(Just Table{})) U{..} =
      Right (d,t{
               tableUQs = tableUQs ++ [UQConstraint{uqAttributeNames = uAttributeNames, uqStyleName = Nothing, uqComment = Nothing }]
              })
    step (d, t@(Just Table{})) F{..} =
      Right (d,t{
               tableFKs = tableFKs ++ [FKConstraint{fkReferencedTableName = fReferencedTableName, fkAttributeMapping = zip fAttributeNames1 fAttributeNames2 {- TODO: fail when lengths do not match! -}, fkStyleName = Nothing, fkComment = Nothing }]
              })
    step (d, t@(Just Table{})) C{..} =
      Right (d,t{
               tableCKs = tableCKs ++ [CKConstraint{ckSQLCondition = cSQLCondition, ckStyleName = Nothing, ckComment = Nothing }]
              })
