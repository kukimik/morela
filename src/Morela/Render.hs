{-# LANGUAGE OverloadedStrings #-}

module Morela.Render (diagramToDotGraph) where

import qualified Data.GraphViz.Attributes as R
import qualified Data.GraphViz.Attributes.Complete as A
import qualified Data.GraphViz.Attributes.HTML as H
import qualified Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Monadic as T
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text.Lazy as L
import qualified Morela.Types as MR

diagramToDotGraph :: MR.Diagram -> G.DotGraph L.Text
diagramToDotGraph d = T.digraph' $ do
  T.graphAttrs
    [ A.RankDir A.FromLeft,
      A.Splines A.SplineEdges,
      A.Overlap A.VoronoiOverlap,
      --A.NodeSep 1,
      A.ESep (A.DVal 0.1),
      --A.Sep (A.DVal 1),
      --A.MinDist 1,
      A.Layout A.Neato,
      A.Mode A.Hier,
      A.Model A.SubSet
    ]
  T.nodeAttrs
    [ A.Shape A.PlainText
    ]
  T.edgeAttrs
    [ A.Color [A.toWC $ A.toColor R.Gray50], -- easier to read labels
      --A.MinLen 2, -- give some breathing room
      A.Style [A.SItem A.Solid []]
    ]
  mapM_ tableToDot (MR.diagramTables d)

tableToDot :: MR.Table -> T.Dot L.Text
tableToDot tab = tableToNode tab >> tableToEdges tab

tableToNode :: MR.Table -> T.Dot L.Text
tableToNode tab = node (MR.tableName tab) [R.toLabel . tableToHTMLLabel $ tab]

tableToEdges :: MR.Table -> T.Dot L.Text
tableToEdges tab =
  mapM_
    (constraintsToEdge (MR.tableName tab) (MR.tableNNs tab) (MR.tableUQs tab) (MR.tablePK tab))
    (MR.tableFKs tab)

constraintsToEdge ::
  MR.TableName ->
  [MR.NNConstraint] ->
  [MR.UQConstraint] ->
  MR.PKConstraint ->
  MR.FKConstraint ->
  T.Dot L.Text
constraintsToEdge tabName nns uqs pk fk =
  edge
    tabName
    (MR.fkReferencedTableName fk)
    [ A.Dir A.Both,
      A.ArrowHead $ A.AType [(A.noMods, A.Normal)],
      A.ArrowTail $ A.AType [(A.noMods, arrowTailType)],
      -- ,A.HeadPort () -- opracuj jak to ładnie zrobić, żeby się do odpowiednich więzów doklejało! zmiana typów...
      --,A.TailPort $ A.LabelledPort (toPortName tabName $ fkToText fk) (Just A.NoCP)
      A.Style [A.SItem edgeStyle []]
    ]
  where
    attrs :: Set.Set MR.AttributeName
    attrs = Set.fromList . fmap fst . MR.fkAttributeMapping $ fk
    pkAttrs = Set.fromList . MR.pkAttributeNames $ pk
    nnAttrs = Set.fromList . fmap MR.nnAttributeName $ nns
    uqsAttrs = fmap (Set.fromList . MR.uqAttributeNames) uqs
    isNN = attrs `Set.isSubsetOf` (pkAttrs `Set.union` nnAttrs)
    isUQ = (`Set.isSubsetOf` attrs) `any` (pkAttrs : uqsAttrs)
    arrowTailType
      | isUQ = A.NoArrow
      | otherwise = A.Crow
    edgeStyle
      | isNN = A.Solid
      | otherwise = A.Dashed

tableToHTMLLabel :: MR.Table -> H.Label
tableToHTMLLabel tab =
  H.Table
    H.HTable
      { H.tableFontAttrs = Nothing,
        H.tableAttrs = [H.CellBorder 0, H.Border 1],
        H.tableRows =
          [headerRow tabName]
            <> [H.HorizontalRule]
            <> attributeRows
            <> [H.HorizontalRule]
            <> [pkRow tabName $ MR.tablePK tab]
            <> uqRows
            <> ckRows
            <> fkRows
            <> indexRows
      }
  where
    tabName = MR.tableName tab
    attributeRows = attributeRow (MR.tablePK tab) (MR.tableNNs tab) <$> MR.tableAttributes tab
    uqRows = uqRow tabName <$> MR.tableUQs tab
    ckRows = ckRow <$> MR.tableCKs tab
    fkRows = fkRow tabName <$> MR.tableFKs tab
    indexRows = indexRow <$> MR.tableIndexes tab

attributeRow :: MR.PKConstraint -> [MR.NNConstraint] -> MR.Attribute -> H.Row
attributeRow pk nns attr =
  H.Cells
    [ H.LabelCell [H.Align H.HLeft]
        . H.Text
        . maybeAddFormat pkFormat
        . maybeAddFormat nnFormat
        $ [H.Str attrName],
      H.LabelCell [H.Align H.HLeft] $
        H.Text [H.Str attrType]
    ]
  where
    attrType = fromMaybe "(?)" (MR.attributeType attr)
    attrName :: MR.AttributeName
    attrName = MR.attributeName attr
    isPK = attrName `elem` MR.pkAttributeNames pk
    isNN = isPK || (attrName ==) `any` map MR.nnAttributeName nns
    pkFormat
      | isPK = Just H.Underline
      | otherwise = Nothing
    nnFormat
      | isNN = Just H.Bold
      | otherwise = Nothing

headerRow :: MR.TableName -> H.Row
headerRow tn =
  H.Cells
    [ H.LabelCell [H.ColSpan 2, H.Align H.HCenter]
        . H.Text
        $ addFormat H.Bold [H.Str tn]
    ]

pkRow :: MR.TableName -> MR.PKConstraint -> H.Row
pkRow tn pk = oneCellRow [H.Port $ toPortName tn txt] txt
  where
    txt = pkToText pk

pkToText :: MR.PKConstraint -> L.Text
pkToText pk = "PK(" <> toCSV (MR.pkAttributeNames pk) <> ")"

uqRow :: MR.TableName -> MR.UQConstraint -> H.Row
uqRow tn uq = oneCellRow [H.Port $ toPortName tn txt] txt
  where
    txt = uqToText uq

uqToText :: MR.UQConstraint -> L.Text
uqToText uq = "UQ(" <> toCSV (MR.uqAttributeNames uq) <> ")"

ckRow :: MR.CKConstraint -> H.Row
ckRow ck = oneCellRow [] $ "CK(" <> MR.ckSQLCondition ck <> ")"

fkRow :: MR.TableName -> MR.FKConstraint -> H.Row
fkRow tn fk = oneCellRow [H.Port $ toPortName tn txt] txt
  where
    txt = fkToText fk

fkToText :: MR.FKConstraint -> L.Text
fkToText fk =
  "FK(" <> toCSV a1 <> ") REFERENCES "
    <> MR.fkReferencedTableName fk
    <> "("
    <> toCSV a2
    <> ")"
  where
    (a1, a2) = unzip $ MR.fkAttributeMapping fk

indexRow :: MR.Index -> H.Row
indexRow ix = oneCellRow [] $
  (if MR.ixIsUnique ix then "UX" else "IX") <>
  "(" <>
  toCSV (MR.ixAttributeNames ix) <>
  ")"

toPortName :: MR.TableName -> L.Text -> A.PortName
toPortName tn txt = A.PN $ prefixTableName tn txt

toCSV :: [L.Text] -> L.Text
toCSV = L.intercalate ","

prefixTableName :: MR.TableName -> L.Text -> L.Text
prefixTableName tabName txt = tabName <> "." <> txt

oneCellRow :: H.Attributes -> L.Text -> H.Row
oneCellRow cellAttr txt =
  H.Cells
    [ H.LabelCell (H.ColSpan 2 : H.Align H.HLeft : cellAttr) $ H.Text [H.Str txt]
    ]

addFormat :: H.Format -> H.Text -> H.Text
addFormat f t = [H.Format f t]

maybeAddFormat :: Maybe H.Format -> H.Text -> H.Text
maybeAddFormat (Just f) = addFormat f
maybeAddFormat Nothing = id
