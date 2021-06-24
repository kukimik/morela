module Morela.Style () where

import qualified Data.GraphViz.Attributes.Complete as A
import qualified Data.GraphViz.Attributes.Colors as C
import qualified Data.GraphViz.Attributes.HTML as H

data CommentStyle =
  CommentNone | CommentTooltip | CommentTableCell | CommentBoth

data WordWrap =
  WordWrapOff | WordWrapColumn Word8

type FontStyle =
  H.Style -> Bool

data Style =
  Style
    {
     graphLayout :: A.GraphvizCommand
    ,graphEdgeType :: A.EdgeType
    ,graphNodeSep :: Double
    ,graphESep :: Double
    ,graphSep :: Double
    ,graphMinDist :: Double
    ,graphModeType :: A.ModeType
    ,graphModel :: A.Model
    ,tableShape :: A.Shape
    ,tableHeaderBgColor :: C.Color
    ,tableBgColor :: C.Color
    ,tableBorder :: Word8
    ,tableCellPadding :: Word8
    ,tableCellBorder :: Word8
    ,tableCommentStyle :: CommentStyle
    ,tableCommentWordWrap :: WordWrap
    ,tableCommentFontStyle :: FontStyle
    ,tableCommentFontSize :: Double
    ,tableFontFace :: String
    ,tableFontSize :: Double
    ,tableHeaderFontFace :: String
    ,tableHeaderFontStyle :: FontStyle
    ,tableHeaderFontSize :: Double
    ,tableNNSymbol :: String
    ,tableNNFontStyle :: FontStyle
    ,tableNNBgColor :: C.Color
    ,tableNNFgColor :: C.Color
    ,tablePKSymbol :: String
    ,tablePKFontStyle :: FontStyle
    ,tablePKBgColor :: C.Color
    ,tablePKFgColor :: C.Color
    ,attributeBgColor :: C.Color
    ,attributeFgColor :: C.Color
    ,attributeTypePlaceholder :: String
    ,attributeFontStyle :: FontStyle
    ,attributeFontSize :: Double
    ,attributeCommentStyle :: CommentStyle
    ,attributeCommentWordWrap :: WordWrap
    ,attributeCommentFontStyle :: FontStyle
    ,attributeCommentFontSize :: Double
    ,constraintWordWrap :: WordWrap
    ,constraintFgColor :: C.Color
    ,constraintBgColor :: C.Color
    ,constraintEdgeColor :: C.Color
    }

