{-# LANGUAGE OverloadedStrings #-}

module Morela.Writer.SQL (diagramToSQL) where

import Data.List (intersperse)
import qualified Data.Set as Set
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder
import qualified Morela.Types as MR

diagramToSQL :: MR.Diagram -> T.Text
diagramToSQL diag = toLazyText . mconcat $
  [bldCreateTable, bldAddForeignKeys, bldCreateIndexes, bldComments] <*>
    Set.toList (MR.diagramTables diag)

bldCreateTable :: MR.Table -> Builder
bldCreateTable MR.Table {MR.tableName = tn, MR.tableAttributes = attrs, MR.tablePK = pk, MR.tableNNs = nns, MR.tableCKs = cks, MR.tableUQs = uqs} =
  mconcat
    [ textsToBld ["CREATE TABLE ", tn, "\n(\n"],
      bldIntercalate (fromLazyText ",\n") $
        (bldAttr <$> attrs) <>
        bldPK <>
        (bldUQ <$> uqs) <>
        (bldCK <$> cks)
     ,fromLazyText "\n);\n\n"
    ]
  where
    bldAttr attr = mconcat
      [
       fromLazyText $ MR.attributeName attr
      ,singleton ' '
      ,case MR.attributeType attr of
         Just t -> fromLazyText t
         Nothing -> mempty
      ,if MR.attributeName attr `elem` fmap MR.nnAttributeName nns
         then fromLazyText " NOT NULL"
         else mempty
      ]
    bldPK
      | [] == MR.pkAttributeNames pk = []
      | otherwise = [mconcat
                      [
                       fromLazyText "PRIMARY KEY("
                      ,bldCSV $ fromLazyText <$> MR.pkAttributeNames pk
                      ,singleton ')'
                      ]]
    bldCK ck = mconcat
                [
                 fromLazyText "CHECK("
                ,fromLazyText $ MR.ckSQLCondition ck
                ,singleton ')'
                ]
    bldUQ uq = mconcat
                [
                 fromLazyText "UNIQUE("
                ,bldCSV $ fromLazyText <$> MR.uqAttributeNames uq
                ,singleton ')'
                ]

bldAddForeignKeys :: MR.Table -> Builder
bldAddForeignKeys MR.Table {MR.tableFKs = fks, MR.tableName = tn} =
  mconcat $ bldAddForeignKey tn <$> fks

bldCreateIndexes :: MR.Table -> Builder
bldCreateIndexes MR.Table {MR.tableIndexes = ixs, MR.tableName = tn} =
  mconcat $ bldCreateIndex tn <$> ixs

bldAddForeignKey :: MR.TableName -> MR.FKConstraint -> Builder
bldAddForeignKey tn fk =
  mconcat
    [ textsToBld ["ALTER TABLE ", tn, " ADD FOREIGN KEY("],
      bldCSV $ fromLazyText <$> c1s,
      textsToBld [") REFERENCES ", MR.fkReferencedTableName fk, "("],
      bldCSV $ fromLazyText <$> c2s,
      fromLazyText ");\n"
    ]
  where
    (c1s, c2s) = unzip $ MR.fkAttributeMapping fk

bldCreateIndex :: MR.TableName -> MR.Index -> Builder
bldCreateIndex tn ix =
  mconcat
    [ fromLazyText "CREATE ",
      if MR.ixIsUnique ix then fromLazyText "UNIQUE " else mempty,
      textsToBld ["INDEX ON ", tn, "("],
      bldCSV $ fromLazyText <$> MR.ixAttributeNames ix,
      fromLazyText ");\n"
    ]

bldComments :: MR.Table -> Builder
bldComments MR.Table {MR.tableName = tn, MR.tableAttributes = as, MR.tableComment = tc} =
  bldTableComment tn tc <> mconcat (bldAttributeComment tn <$> as)

bldTableComment :: MR.TableName -> Maybe MR.Comment -> Builder
bldTableComment tn (Just c) =
  textsToBld ["COMMENT ON TABLE ", tn, " IS \"", c, "\";\n"]
bldTableComment _ _ = mempty

bldAttributeComment :: MR.TableName -> MR.Attribute -> Builder
bldAttributeComment tn MR.Attribute{MR.attributeName = an, MR.attributeComment = Just c} =
  textsToBld ["COMMENT ON COLUMN ", tn, ".", an, " IS \"", c, "\";\n"]
bldAttributeComment _ _ = mempty

textsToBld :: [T.Text] -> Builder
textsToBld = mconcat . fmap fromLazyText

bldCSV :: [Builder] -> Builder
bldCSV = bldIntercalate $ singleton ','

bldIntercalate :: Builder -> [Builder] -> Builder
bldIntercalate b = mconcat . intersperse b

