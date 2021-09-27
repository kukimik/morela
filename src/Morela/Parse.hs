{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Morela.Parse
  ( toDiagram,
  )
where

import Data.String(fromString,IsString)
import qualified Data.Text.Lazy as T
import Control.Monad (foldM)
import Control.Monad.Except(MonadError,throwError)
import qualified Data.Set as Set
import Morela.Types
import Text.Parsec.Morela.Parser (AST (..))

toDiagram :: forall s m. (IsString s, MonadError s m) => [AST] -> m Diagram
toDiagram xs = do
  diag <- foldM step (emptyDiagram, Nothing) xs >>= uncurry addTable'
  checkConstraints diag
  return diag
  where
    addTable d@Diagram{..} t
      | Set.null . Set.filter ((tableName t ==) . tableName) $ diagramTables =
          pure d {diagramTables = Set.insert t diagramTables}
      | otherwise =
          throwError . fromString $
            "Duplicate table name: " <> T.unpack (tableName t) <> "."
    addTable' d = maybe (pure d) (addTable d)
    checkConstraints _ = pure () -- TODO: check whether constraints are valid
    step :: (Diagram, Maybe Table) -> AST -> m (Diagram, Maybe Table)
    step (d, Nothing) T {..} =
      pure (d, Just emptyTable{tableName = tName, tableComment = tComment})
    step (_, Nothing) _ =
      throwError "Attribute, constraint or index defined before first table."
    step (d, Just t@Table{}) ast = ((,)) <$> d' <*> (Just <$> t')
      where
        d' = case ast of
          T {..} -> addTable d t
          _ -> pure d
        t' = case ast of
          T {..} -> pure $ emptyTable{tableName = tName, tableComment = tComment}
          A {..} ->
            -- TODO: the list appending below is non-optimal
            pure $
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
          U {..} ->
            pure $
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
          F {..} ->
            pure $
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
          C {..} ->
            pure $
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
          I {..} ->
            pure $
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
