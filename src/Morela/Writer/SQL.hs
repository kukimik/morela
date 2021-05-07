{-# LANGUAGE OverloadedStrings #-}

module Morela.Writer.SQL (diagramToSQL) where

import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder
import qualified Morela.Types as MR

diagramToSQL :: MR.Diagram -> L.Text
diagramToSQL =
  toText .
  mconcat .
  (uncurry (<>)) .
  unzip .
  fmap (
        bldCreateTable
        &&&
        bldAddForeignKeys
       ) .
  diagramTables

bldCreateTable :: MR.Table -> Builder
bldCreateTable = undefined

bldAddForeignKeys :: MR.Table -> Builder
bldAddForeignKeys _{tableFKs=fks,tableName=tn,...} =
  mconcat $ bldAddForeignKey tn <$> fks

bldAddForeignKey :: MR.TableName -> MR.FKConstraint -> Builder
bldAddForeignKey tn fk = mconcat . fmap fromText $
  ["ALTER TABLE "
  ,fromText tn
  ," ADD FOREIGN KEY("
  ,L.intercalate "," c1s -- builder!!!
  ,") REFERENCES "
  ,fkReferencedTableName fk
  ,"("
  ,L.intercalate "," c2s -- builder!!!
  ,");\n"
  ]
  where (c1s,c2s) = fkAttributeMapping fk
