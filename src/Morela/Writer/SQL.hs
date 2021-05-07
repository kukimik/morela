{-# LANGUAGE OverloadedStrings #-}

module Morela.Writer.SQL (diagramToSQL) where

import Control.Arrow ((&&&))
import Data.List (intersperse)
import qualified Data.Set as Set
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder
import qualified Morela.Types as MR

diagramToSQL :: MR.Diagram -> L.Text
diagramToSQL =
  toLazyText
    . mconcat
    . fmap
      ( uncurry (<>)
          . ( bldCreateTable
                &&& bldAddForeignKeys
            )
      )
    . Set.toList
    . MR.diagramTables

bldCreateTable :: MR.Table -> Builder
bldCreateTable MR.Table {MR.tableName = tn, MR.tableAttributes = as, MR.tablePK = pk, MR.tableNNs = nns, MR.tableCKs = cks, MR.tableUQs = uqs} =
  mconcat
    [ textsToBld ["CREATE TABLE ", tn, "(\n"],
      bldAttrs,
      bldPK,
      bldUQs,
      bldCKs,
      fromLazyText ");\n"
    ]
  where
    bldAttrs = undefined
    bldPK = undefined
    bldCKs = undefined
    bldUQs = undefined

bldAddForeignKeys :: MR.Table -> Builder
bldAddForeignKeys MR.Table {MR.tableFKs = fks, MR.tableName = tn} =
  mconcat $ bldAddForeignKey tn <$> fks

bldAddForeignKey :: MR.TableName -> MR.FKConstraint -> Builder
bldAddForeignKey tn fk =
  mconcat
    [ textsToBld ["ALTER TABLE ", tn, " ADD FOREIGN KEY("],
      bldCSV c1s, -- builder!!!
      textsToBld [") REFERENCES ", MR.fkReferencedTableName fk, "("],
      bldCSV c2s, -- builder!!!
      fromLazyText ");\n"
    ]
  where
    (c1s, c2s) = unzip $ MR.fkAttributeMapping fk

textsToBld :: [L.Text] -> Builder
textsToBld = mconcat . fmap fromLazyText

bldCSV :: [L.Text] -> Builder
bldCSV = mconcat . fmap fromLazyText . intersperse ","
