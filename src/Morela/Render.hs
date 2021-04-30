{-# LANGUAGE OverloadedStrings #-}

module Morela.Render
  (diagramToDotGraph) where

import qualified Morela.Types                      as MR

import Data.GraphViz.Attributes(toLabel)
import qualified Data.GraphViz.Attributes.Complete   as A
import qualified Data.GraphViz.Attributes.HTML       as H
import qualified Data.GraphViz.Types.Generalised     as G
import           Data.GraphViz.Types.Monadic         as T

import qualified Data.Text.Lazy                    as L
import           Data.Maybe                        (fromMaybe)
import qualified Data.Set as Set

diagramToDotGraph :: MR.Diagram -> G.DotGraph L.Text
diagramToDotGraph = T.digraph' . (mapM_ tableToDot) . MR.diagramTables

tableToDot :: MR.Table -> T.Dot L.Text
tableToDot tab = tableToNode tab >> tableToEdges tab

tableToNode :: MR.Table -> T.Dot L.Text
tableToNode tab = node (MR.tableName tab) [toLabel . tableToHTMLLabel $ tab]

tableToEdges :: MR.Table -> T.Dot L.Text
tableToEdges tab =
  mapM_
    (constraintsToEdge (MR.tableName tab) (MR.tableNNs tab) (MR.tableUQs tab) (MR.tablePK tab))
    (MR.tableFKs tab)

constraintsToEdge ::
     MR.TableName
  -> [MR.NNConstraint]
  -> [MR.UQConstraint]
  -> MR.PKConstraint
  -> MR.FKConstraint
  -> T.Dot L.Text
constraintsToEdge tabName nns uqs pk fk =
  edge
    tabName
    (MR.fkReferencedTableName fk)
    [A.Dir A.Both
    ,A.ArrowHead $ A.AType [(A.noMods, A.Normal)]
    ,A.ArrowTail $ A.AType [(A.noMods, arrowTailType)]
    -- ,A.HeadPort () -- opracuj jak to ładnie zrobić, żeby się do odpowiednich więzów doklejało! zmiana typów...
    ,A.TailPort $ A.LabelledPort (toPortName tabName $ fkToText fk) Nothing
    ,A.Style [A.SItem edgeStyle []]
    ]
  where
    attrs :: Set.Set MR.AttributeName
    attrs = Set.fromList . (fmap fst) . MR.fkAttributeMapping $ fk
    pkAttrs = Set.fromList . MR.pkAttributeNames $ pk
    nnAttrs = Set.fromList . (fmap MR.nnAttributeName) $ nns
    uqsAttrs = fmap (Set.fromList . MR.uqAttributeNames) uqs
    isNN = attrs `Set.isSubsetOf` (pkAttrs `Set.union` nnAttrs)
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
                       [headerRow tabName]
                    <> attributeRows
                    <> [H.HorizontalRule]
                    <> [pkRow tabName $ MR.tablePK tab]
                    <> uqRows
                    <> ckRows
                    <> fkRows
                 }
  where
    tabName = MR.tableName tab
    attributeRows = attributeRow (MR.tablePK tab) (MR.tableNNs tab) <$> MR.tableAttributes tab
    uqRows = uqRow tabName <$> MR.tableUQs tab
    ckRows = ckRow <$> MR.tableCKs tab
    fkRows = fkRow tabName <$> MR.tableFKs tab

attributeRow :: MR.PKConstraint -> [MR.NNConstraint] -> MR.Attribute -> H.Row
attributeRow pk nns attr =
  H.Cells
    [H.LabelCell []
      . H.Text
      . maybeAddFormat pkFormat
      . maybeAddFormat nnFormat
      $ [H.Str attrName]
    ,H.LabelCell []
      $ H.Text [H.Str attrType]
    ]
  where
    attrType = fromMaybe "(?)" (MR.attributeType attr)
    attrName :: MR.AttributeName
    attrName = MR.attributeName attr
    isPK = attrName `elem` (MR.pkAttributeNames pk)
    isNN = isPK || (attrName ==) `any` (map MR.nnAttributeName nns)
    pkFormat
      | isPK = Just H.Underline
      | otherwise = Nothing
    nnFormat
      | isNN = Just H.Bold
      | otherwise = Nothing

headerRow :: MR.TableName -> H.Row
headerRow tn =
  H.Cells
   [ H.LabelCell [H.ColSpan 2]
     . H.Text
     $ addFormat H.Bold [H.Str tn]
   ]

pkRow :: MR.TableName -> MR.PKConstraint -> H.Row
pkRow tn pk = oneCellRow [H.Port $ toPortName tn txt] $ txt
  where txt = pkToText pk

pkToText :: MR.PKConstraint -> L.Text
pkToText pk = "PK(" <> (toCSV (MR.pkAttributeNames pk)) <> ")"

uqRow :: MR.TableName -> MR.UQConstraint -> H.Row
uqRow tn uq = oneCellRow [H.Port $ toPortName tn txt] $ txt -- add table name to port name! (or maybe better unique table id?)
  where txt = uqToText uq

uqToText :: MR.UQConstraint -> L.Text
uqToText uq = "UQ(" <> (toCSV (MR.uqAttributeNames uq)) <> ")"

ckRow :: MR.CKConstraint -> H.Row
ckRow ck = oneCellRow [] $ "CK(" <> (MR.ckSQLCondition ck) <> ")"

fkRow :: MR.TableName -> MR.FKConstraint -> H.Row
fkRow tn fk = oneCellRow [H.Port $ toPortName tn txt] $ txt -- add table name to port name! (or maybe better unique table id?)
  where txt = fkToText fk

fkToText :: MR.FKConstraint -> L.Text
fkToText fk = "FK(" <> (toCSV a1) <> ") REFERENCES " <> (MR.fkReferencedTableName fk) <> "(" <> (toCSV a2) <> ")"
  where (a1,a2) = unzip $ MR.fkAttributeMapping fk

toPortName :: MR.TableName -> L.Text -> A.PortName
toPortName tn txt = A.PN $ prefixTableName tn txt

toCSV :: [L.Text] -> L.Text
toCSV = L.intercalate ","

prefixTableName :: MR.TableName -> L.Text -> L.Text
prefixTableName tabName txt = tabName <> "." <> txt

oneCellRow :: H.Attributes -> L.Text -> H.Row
oneCellRow cellAttr txt = H.Cells [ H.LabelCell ((H.ColSpan 2):cellAttr) $ H.Text [H.Str txt] ]

addFormat :: H.Format -> H.Text -> H.Text
addFormat f t = [H.Format f t]

maybeAddFormat :: Maybe H.Format -> H.Text -> H.Text
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
