{-# LANGUAGE OverloadedStrings #-}

module Morela.Render
  (diagramToDotGraph) where

import           Control.Monad                     (forM_, mapM_)
import qualified Morela.Types                      as MR

import qualified Data.GraphViz.Attributes.Colors.X11 as C
import qualified Data.GraphViz.Attributes.Complete   as A
import qualified Data.GraphViz.Attributes.HTML       as H
import qualified Data.GraphViz.Types.Generalised     as G
import           Data.GraphViz.Types.Monadic         as T

import qualified Data.Text.Lazy                    as L
import           Text.Printf                       (printf)
import           Data.Maybe                        (fromMaybe,maybeToList)
import           Data.Set as Set (fromList)

diagramToDotGraph :: MR.Diagram -> G.DotGraph L.Text
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

constraintsToEdge ::
     MR.TableName
  -> [MR.NNConstraint]
  -> [MR.UQConstraint]
  -> MR.PKConstraint
  -> MR.FKConstraint
  -> G.Dot L.Text
constraintsToEdge tabName nns uqs pk fk =
  edge
    tabName
    (fkReferencedTableName fk)
    [A.Dir Both
    ,A.ArrowHead $ A.AType [(A.noMods, A.Normal)]
    ,A.ArrowTail $ A.AType [(A.noMods, arrowTailType)]
    -- ,A.HeadPort () -- opracuj jak to ładnie zrobić, żeby się do odpowiednich więzów doklejało! zmiana typów...
    ,A.TailPort $ A.LabelledPort (prefix tabName $ fkToText fk) Nothing
    ,A.Style [A.SItem edgeStyle []]
    ]
  where
    attrs = Set.fromList . fst . fkAttributeMapping $ fk
    pkAttrs = Set.fromList $ pkAttributeNames pks
    nnAttrs = Set.fromList $ fmap nnAttributeName nns
    uqsAttrs = fmap (Set.fromList . uqAttributeNames) uqs
    isNN = attrs `Set.isSubsetOf` (pkAttrs `union` nnAttrs)
    isUQ = (`Set.isSubsetOf` attrs) `any` (pkAttrs:uqsAttrs)
    arrowTailType
      | isUQ = A.NoArrow
      | otherwise = A.Crow
    edgeStyle
      | isNN = A.Solid
      | otherwise = A.Dashed

tableToHTMLLabel :: MR.Table -> H.Label
tableToHTMLLabel tab = H.Table H.HTable
                 { H.tableFontAttrs = Nothing
                 , H.tableAttrs = [H.CellBorder 0, H.Border 1]
                 , H.tableRows =
                       [headerRow $ MR.tableName tab]
                    <> attributeRows
                    <> [H.HorizontalRule]
                    <> pkRows
                    <> uqRows
                    <> ckRows
                    <> fkRows
                 }
  where
    attributeRows = map (attributeRow (MR.tablePK tab) (MR.tableNNs tab)) $ MR.tableAttributes tab
    pkRows = map (pkRow $ MR.tableName tab) $ maybeToList (MR.tablePK tab)
    uqRows = map (uqRow $ MR.tableName tab) $ MR.tableUQs tab
    ckRows = map ckRow $ MR.tableCKs tab
    fkRows = map (fkRow $ MR.tableName tab) $ MR.tableFKs tab

attributeRow :: MR.PKConstraint -> [MR.NNConstraints] -> MR.Attribute -> H.Row
attributeRow pk nns attr =
  H.Cells
    [H.LabelCell
      $ maybeAddFormat pkFormat
      $ maybeAddFormat nnFormat
      $ [H.Str attrName]
    ,H.LabelCell $ [H.Str attributeType attr]
    ]
  where
    attrName = attributeName attr
    isPK = attrName `elem` pkAttributeNames pk
    isNN = isPK || (attrName `elem`) `any` (map nnAttributeNames nns)
    pkFormat
      | isPK = Just H.Underline
      | otherwise = Nothing
    nnFormat =
      | isPK = Just Just H.Bold
      | otherwise = Nothing

headerRow :: MR.TableName -> H.Row
headerRow tabName = H.Cells [ H.LabelCell (H.ColSpan 2) $ addFormat H.Bold [H.Str txt] ]

pkRow :: MR.TableName -> MR.PKConstraint -> H.Row
pkRow tn pk oneCellRow [H.Port $ prefixTableName tn txt ] $ txt
  where txt = pkToText pk

pkToText :: MR.PKConstraint -> L.Text
pkToText pk = "PK(" <> (toCSV (MR.pkAttributeNames pk)) <> ")"

uqRow :: MR.TableName -> MR.UQConstraint -> H.Row
uqRow tn uq = oneCellRow [H.Port $ prefixTableName tn txt ] $ txt -- add table name to port name! (or maybe better unique table id?)

uqToText :: MR.PKConstraint -> L.Text
uqToText uq = "UQ(" <> (toCSV (MR.uqAttributeNames pk)) <> ")"

ckRow :: MR.CKConstraint -> H.Row
ckRow ck = oneCellRow [] $ "CK(" <> (MR.ckSQLCondition ck) <> ")"

fkRow :: MR.TableName -> MR.FKConstraint -> H.Row
fkRow tn fk = oneCellRow [H.Port $ prefixTableName tn txt ] $ txt -- add table name to port name! (or maybe better unique table id?)
  where txt = fkToText fk

fkToText :: MR.FKConstraint -> L.Text
fkToText fk = "FK(" <> (toCSV a1) <> ") REFERENCES " <> (fkReferencedTableName fk) <> "(" <> (toCSV a2) <> ")"
  where (a1,a2) = unzip $ fkAttributeMapping fk

toCSV :: [L.Text] -> L.Text
toCSV = L.intercalate ","

prefixTableName :: MR.TableName -> L.Text -> L.Text
prefixTableName tabName txt = tabName <> "." <> txt

oneCellRow :: H.Attributes -> L.Text -> H.Row
oneCellRow cellAttr txt = H.Cells [ H.LabelCell (H.ColSpan 2):cellAttr $ [H.Str txt] ]

addFormat :: H.Format -> H.Text
addFormat f t = [H.Format f t]

maybeAddFormat :: Maybe H.Format -> H.Text
maybeAddFormat (Just f) = addFormat f
maybeAddFormat Nothing = id

{-
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
-}
