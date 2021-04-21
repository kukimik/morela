{-# LANGUAGE OverloadedStrings #-}

module Morela.Render
  (htmlAttr,
   htmlFont,
   recordAttr,
   withLabelFmt
  ) where

import           Control.Monad                     (forM_, mapM_)
import qualified Morela.Types                      as MR

import qualified Data.GraphViz.Attributes.Colors.X11 as C
import qualified Data.GraphViz.Attributes.Complete   as A
import qualified Data.GraphViz.Attributes.HTML       as H
import qualified Data.GraphViz.Types.Generalised     as G
import           Data.GraphViz.Types.Monadic         as T

import qualified Data.Text.Lazy                    as L
import           Text.Printf                       (printf)
import           Data.Maybe                        (fromMaybe)



diagramToDotGraph :: MR.Morela -> G.DotGraph L.Text
diagramToDotGraph = T.digraph' . (mapM_ tableToDot) . diagramTables

tableToDot :: MR.Table -> G.Dot L.Text
tableToDot tab = tableToNode tab >> tableToEdges tab

tableToNode :: MR.Table -> G.Dot L.Text
tableToNode tab = node (MR.tableName tab) [tableToHTMLLabel tab]

tableToEdges :: MR.Table -> G.Dot L.Text
tableToEdges tab =
  mapM_
    (constraintsToEdge (MR.tableNNs tab) (MR.tableUQs tab) (MR.tablePK tab))
    (MR.tableFKs tab)

constraintsToEdge :: MR.TableName -> [MR.NNConstraint] -> [MR.UQConstraint] -> MR.PKConstraint -> MR.FKConstraint -> G.Dot L.Text
constraintsToEdge tabName nns uqs pks fk = edge .........



tableToHTMLLabel :: MR.Table -> H.Label
tableToHTMLLabel tab = H.Table H.HTable
                 { H.tableFontAttrs = Nothing
                 , H.tableAttrs = []
                 , H.tableRows =
                       [headerRow]
                    <> attributeRows
                    <> [H.HorizontalRule]
                    <> uqRows
                    <> ckRows
                    <> fkRows
                 }
  where
    attributeRows = map (attributeRow (MR.tablePK tab) (MR.tableNNs tab)) $ MR.tableAttributes tab
    uqRows = map uqRow $ MR.tableUQs tab
    ckRows = map ckRow $ MR.tableCKs tab
    fkRows = map fkRow $ MR.tableFKs tab

attributeRow :: MR.PKConstraint -> [MR.NNConstraints] -> MR.Attribute -> H.Row
attributeRow attr =
  H.Cells
    [H.LabelCell $ [attributeName attr]
    ,H.LabelCell $ [H.Str attributeType attr]
    ]
  where

headerRow :: MR.TableName -> H.Row
headerRow tabName = H.Cells [ H.LabelCell (H.ColSpan 2) $ addFormat H.Bold [H.Str txt] ]

uqRow :: MR.UQConstraint -> H.Row
uqRow uq = oneCellRow [] $ "UQ(" <> (toCSV (MR.uqAttributeNames uq)) <> ")"

ckRow :: MR.CKConstraint -> H.Row
ckRow ck = oneCellRow [] $ "CK(" <> (MR.ckSQLCondition ck) <> ")"

fkRow :: MR.FKConstraint -> H.Row
fkRow fk = oneCellRow [H.Port $ fkToText fk ] $ fkToText fk

fkToText :: MR.FKConstraint -> L.Text
fkToText fk = "FK(" <> (toCSV c1) <> ") REFERENCES " <> (fkReferencedTableName fk) <> "(" <> (toCSV c2) <> ")"
  where (c1,c2) = unzip $ fkAttributeMapping fk

toCSV :: [L.Text] -> L.Text
toCSV = L.intercalate ","

oneCellRow :: H.Attributes -> L.Text -> H.Row
oneCellRow cellAttr txt = H.Cells [ H.LabelCell (H.ColSpan 2):cellAttr $ [H.Str txt] ]

addFormat :: H.Format -> H.Text
addFormat f t = [H.Format f t]


  graph' $ do
  graphAttrs (graphTitle $ title er)
  graphAttrs [ A.RankDir A.FromLeft
             , A.Splines $ fromMaybe (fromJust . edgeType $ defaultConfig) (edgeType conf)
             ]
  nodeAttrs nodeGlobalAttributes
  edgeAttrs [ A.Color [A.toWC $ A.toColor C.Gray50] -- easier to read labels
            , A.MinLen 2 -- give some breathing room
            , A.Style [A.SItem A.Dashed []] -- easier to read labels, maybe?
            ]
  forM_ (entities er) $ \e ->
    node (name e) [entityFmt e]
  forM_ (rels er) $ \r -> do
    let optss    = roptions r
        rlab     = A.HtmlLabel . H.Text . htmlFont optss . L.pack . show
        (l1, l2) = (A.TailLabel $ rlab $ card1 r, A.HeadLabel $ rlab $ card2 r)
        label    = A.Label $ A.HtmlLabel $ H.Text $ withLabelFmt " %s " optss []
    edge (entity1 r) (entity2 r) [label, l1, l2]
    where nodeGlobalAttributes
            | dotentity conf = [shape Record, A.RankDir A.FromTop]
            | otherwise = [shape PlainText] -- recommended for HTML labels
          entityFmt
            | dotentity conf = toLabel . dotEntity
            | otherwise = toLabel . htmlEntity



-- | Extracts and formats a graph title from the options given.
-- The options should be title options from an ER value.
-- If a title does not exist, an empty list is returned and `graphAttrs attrs`
-- should be a no-op.
graphTitle :: Options -> [A.Attribute]
graphTitle topts =
  let glabel = optionsTo optToLabel topts
  in if null glabel then [] else
       [ A.LabelJust A.JLeft
       , A.LabelLoc A.VTop
       , A.Label $ A.HtmlLabel $ H.Text $ htmlFont topts (head glabel)
       ]

checkRequirements :: IO ()
checkRequirements = (isGraphvizInstalled >>= guard) <|> quitWithoutGraphviz msg
  where
    msg = "GraphViz is not installed on your system.\n" ++
          "Please install it first, https://github.com/BurntSushi/erd"



-- | Converts a single attribute to an HTML table row.
htmlAttr :: ER.Attribute -> H.Row
htmlAttr ER.Separator = H.HorizontalRule
htmlAttr a = H.Cells [cell,tcell]
  where cell     = H.LabelCell cellAttrs (H.Text $ withLabelFmt " [%s]" opts name)
        tcell    = H.LabelCell cellAttrs (H.Text $ typename)
        typename = htmlFont opts (fromMaybe " " $ ER.datatype a)
        name     = nnfmt $ fkfmt $ pkfmt $ htmlFont opts (ER.field a)
        pkfmt s  = if ER.pk a then [H.Format H.Underline s] else s
        fkfmt s  = if ER.fk a then [H.Format H.Italics s] else s
        nnfmt s  = if ER.nn a then [H.Format H.Bold s] else s
        opts     = ER.aoptions a
        cellAttrs = ER.optionsTo ER.optToHtml opts
-- | Converts a single attribute to a RecordField ( an element of a dot table )
recordAttr :: ER.Attribute -> A.RecordField
recordAttr a = A.FieldLabel $ ER.field a -- should change to add port support!
-- | Formats an arbitrary string with the options given (using only font
-- attributes).
htmlFont :: ER.Options -> L.Text -> H.Text
htmlFont opts s = [H.Font (ER.optionsTo ER.optToFont opts) [H.Str s]]

-- | Formats HTML text with a label. The format string given should be
-- in `Data.Text.printf` style. (Only font options are used from the options
-- given.)
withLabelFmt :: String -> ER.Options -> H.Text -> H.Text
withLabelFmt fmt opts s =
  case ER.optionsTo ER.optToLabel opts of
    (x:_) -> s ++ htmlFont opts (L.pack $ printf fmt $ L.unpack x)
    _     -> s
