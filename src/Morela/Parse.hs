{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Morela.Parse
  ( toDiagram,
  )
where

import Control.Monad (foldM)
import qualified Data.Set as Set
import Morela.Types
import Text.Parsec.Morela.Parser (AST (..))

toDiagram :: [AST] -> Either String Diagram
toDiagram xs = do
  diag <- uncurry addMaybeTable <$> foldM step (emptyDiagram, Nothing) xs
  checkConstraints diag
  return diag
  where
    addMaybeTable :: Diagram -> Maybe Table -> Diagram
    addMaybeTable d Nothing = d
    addMaybeTable d@Diagram {..} (Just t) = d {diagramTables = Set.insert t diagramTables} -- TODO: fail on duplicate table name
    checkConstraints :: Diagram -> Either String ()
    checkConstraints _ = Right () -- TODO: check whether constraints are valid
    step :: (Diagram, Maybe Table) -> AST -> Either String (Diagram, Maybe Table)
    step (d, Nothing) T {..} = Right (d, Just emptyTable{tableName = tName, tableComment = tComment})
    step (_, Nothing) _ = Left "Attribute or constraint comes before first table."
    step (d, t@(Just Table {})) T {..} =
      Right
        ( addMaybeTable d t,
          Just emptyTable{tableName = tName, tableComment = tComment}
        )
    step (d, Just t@Table {}) A {..} =
      -- TODO: the list appending below is evil, fix this
      Right
        ( d,
          Just
            t
              { tableAttributes =
                  tableAttributes t
                    <> [ Attribute
                           { attributeName = aName,
                             attributeType = aTypeName,
                             attributeComment = aComment,
                             attributeStyleName = Nothing
                           }
                       ],
                tableNNs =
                  if aIsNN
                    then
                      tableNNs t
                        <> [NNConstraint {nnAttributeName = aName}]
                    else tableNNs t,
                tablePK =
                  if aIsPK
                    then
                      (tablePK t)
                        { pkAttributeNames =
                            pkAttributeNames (tablePK t)
                              <> [aName]
                        }
                    else tablePK t -- TODO: make this readable
              }
        )
    step (d, Just t@Table {}) U {..} =
      Right
        ( d,
          Just
            t
              { tableUQs =
                  tableUQs t
                    <> [ UQConstraint
                           { uqAttributeNames = uAttributeNames,
                             uqStyleName = Nothing,
                             uqComment = Nothing
                           }
                       ]
              }
        )
    step (d, Just t@Table {}) F {..} =
      Right
        ( d,
          Just
            t
              { tableFKs =
                  tableFKs t
                    <> [ FKConstraint
                           { fkReferencedTableName = fReferencedTableName,
                             fkAttributeMapping = zip fAttributeNames1 fAttributeNames2 {- TODO: fail when lengths do not match! -},
                             fkStyleName = Nothing,
                             fkComment = Nothing
                           }
                       ]
              }
        )
    step (d, Just t@Table {}) C {..} =
      Right
        ( d,
          Just
            t
              { tableCKs =
                  tableCKs t
                    <> [ CKConstraint
                           { ckSQLCondition = cSQLCondition,
                             ckStyleName = Nothing,
                             ckComment = Nothing
                           }
                       ]
              }
        )
    step (d, Just t@Table {}) I {..} =
      Right
        ( d,
          Just
            t
              { tableIndexes =
                  tableIndexes t
                    <> [ Index
                           { ixAttributeNames = iAttributeNames,
                             ixIsUnique = iIsUnique,
                             ixStyleName = Nothing,
                             ixComment = Nothing
                           }
                       ]
              }
        )
