{-# LANGUAGE OverloadedStrings #-}

module Morela.Render (diagramToDotGraph) where

import Control.Monad(join)
import qualified Data.GraphViz.Attributes as R
import qualified Data.GraphViz.Attributes.Complete as A
import qualified Data.GraphViz.Attributes.HTML as H
import qualified Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Monadic as T
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text.Lazy as L
import qualified Morela.Types as MR
import Text.Wrap(wrapTextToLines,defaultWrapSettings)

wrap :: Int -> L.Text -> [L.Text]
wrap n = join . fmap (fmap L.fromStrict . (wrapTextToLines defaultWrapSettings n) . L.toStrict) . L.lines

linesToHTMLText :: [L.Text] -> H.Text
linesToHTMLText = (intersperse (H.Newline [])) . fmap H.Str

wrappedHTMLText :: Int -> L.Text -> H.Text
wrappedHTMLText n = linesToHTMLText . (wrap n)

diagramToDotGraph :: MR.Diagram -> G.DotGraph L.Text
diagramToDotGraph d = T.digraph' $ do
  T.graphAttrs
    [ A.Splines A.SplineEdges,
      A.Overlap A.VoronoiOverlap,
      A.NodeSep 1,
      A.ESep (A.DVal 0.1),
      A.Sep (A.DVal 0),
      A.MinDist 1,
      A.Layout A.Dot,
      A.Mode A.Hier,
      A.Model A.SubSet
      --A.Dim 3
    ]
  T.nodeAttrs
    [ A.Shape A.PlainText
    ]
  T.edgeAttrs
    [ A.Color [A.toWC $ A.toColor R.Gray50], -- easier to read labels
      A.MinLen 2, -- give some breathing room
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
    ([ A.Dir A.Both,
      A.ArrowHead $ A.AType [(A.noMods, A.Vee)],
      A.ArrowTail $ A.AType [(A.noMods, arrowTailType),(A.noMods, A.Tee)],
      A.Style [A.SItem edgeStyle []]
 --    ,A.TailPort $ A.LabelledPort (toPortName tabName $ fkToText fk) (Just A.NoCP)
    ]
    -- <>
    -- if tabName == MR.fkReferencedTableName fk
    --   then [A.HeadPort $ A.CompassPoint A.East, A.TailPort $ A.CompassPoint A.North ]
    --  else []
    )
  where
    attrs :: Set.Set MR.AttributeName
    attrs = Set.fromList . fmap fst . MR.fkAttributeMapping $ fk
    pkAttrs = Set.fromList . MR.pkAttributeNames $ pk
    nnAttrs = Set.fromList . fmap MR.nnAttributeName $ nns
    uqsAttrs = fmap (Set.fromList . MR.uqAttributeNames) uqs
    isNN = attrs `Set.isSubsetOf` (pkAttrs `Set.union` nnAttrs)
    isUQ = (`Set.isSubsetOf` attrs) `any` (pkAttrs : uqsAttrs)
    arrowTailType
      | isUQ = A.Tee
      | otherwise = A.Crow
    edgeStyle
      | isNN = A.Solid
      | otherwise = A.Dashed

tableToHTMLLabel :: MR.Table -> H.Label
tableToHTMLLabel tab =
  H.Table
    H.HTable
      { H.tableFontAttrs = Just [H.Face "Times-Roman"],
        H.tableAttrs = [H.CellBorder 0, H.CellPadding 4, H.Border 1],
        H.tableRows =
          mconcat
          [
           [headerRow tabName $ fromMaybe tabName (MR.tableComment tab)]
          ,prependHorizontalRule attributeRows
          ,prependHorizontalRule constraintRows
          ,maybe
            []
            (\txt ->
                [H.HorizontalRule
                ,H.Cells
                  [ H.LabelCell [H.ColSpan 3, H.Align H.HLeft, H.BAlign H.HLeft]
                      . H.Text
                      . (:[])
                      . H.Font [H.PointSize 13.0]
                      . wrappedHTMLText 80 $ txt
                  ]]
            )
            (MR.tableComment tab)
          ]
      }
  where
    tabName = MR.tableName tab
    prependHorizontalRule [] = []
    prependHorizontalRule xs = H.HorizontalRule : xs
    attributeRows = attributeRow (MR.tablePK tab) (MR.tableNNs tab) <$> MR.tableAttributes tab
    constraintRows = mconcat [pkRows,uqRows,ckRows,fkRows,indexRows]
    uqRows = uqRow tabName <$> MR.tableUQs tab
    ckRows = ckRow <$> MR.tableCKs tab
    fkRows = fkRow tabName <$> MR.tableFKs tab
    pkRows = (MR.pkAttributeNames $ MR.tablePK tab)
              *> [pkRow tabName $ MR.tablePK tab]
    indexRows = indexRow <$> MR.tableIndexes tab

attributeRow :: MR.PKConstraint -> [MR.NNConstraint] -> MR.Attribute -> H.Row
attributeRow pk nns attr =
  H.Cells
    [ H.LabelCell [H.Align H.HLeft, H.HRef "#", H.Title $ fromMaybe (MR.attributeName attr) (MR.attributeComment attr)]
        . H.Text
        . (:[])
        . H.Font [H.PointSize 14.0]
        . maybeAddFormat pkFormat
        . maybeAddFormat nnFormat
        $ [H.Str attrName],
      H.LabelCell [H.Align H.HLeft]
        . H.Text
        . (:[])
        . H.Font [H.PointSize 14.0]
        $ [H.Str attrType],
      H.LabelCell [H.Align H.HLeft, H.BAlign H.HLeft]
        . H.Text
        . (:[])
        . H.Font [H.PointSize 13.0]
        $ case MR.attributeComment attr of
            Nothing -> [H.Str " "]
            Just txt -> wrappedHTMLText 80 txt
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

headerRow :: MR.TableName -> MR.Comment -> H.Row
headerRow tn cmt =
  H.Cells
    [ H.LabelCell [H.ColSpan 3, H.Align H.HCenter, H.Title cmt, H.HRef "#"]
        . H.Text
        . (:[])
        . H.Font [H.PointSize 22.0]
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
toCSV = L.intercalate ", "

prefixTableName :: MR.TableName -> L.Text -> L.Text
prefixTableName tabName txt = tabName <> "." <> txt

oneCellRow :: H.Attributes -> L.Text -> H.Row
oneCellRow cellAttr txt =
  H.Cells
    [ H.LabelCell (H.ColSpan 3 : H.Align H.HLeft : H.BAlign H.HLeft : cellAttr) $ H.Text (wrappedHTMLText 160 txt)-- [H.Str txt]
    ]

addFormat :: H.Format -> H.Text -> H.Text
addFormat f t = [H.Format f t]

maybeAddFormat :: Maybe H.Format -> H.Text -> H.Text
maybeAddFormat (Just f) = addFormat f
maybeAddFormat Nothing = id
