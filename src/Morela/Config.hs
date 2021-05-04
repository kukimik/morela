{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Morela.Config
  ( Opts (..),
    OutputFormat (..),
    optsParserInfo,
    toGraphvizOutput,
  )
where

import Data.Functor.Identity (Identity (..))
import qualified Data.GraphViz.Commands as GC
import Data.List (intercalate)
import Options.Applicative

data OutputFormat
  = SQL
  | Morela
  | GraphVizDotOutput
  | GraphVizEps
  | GraphVizFig
  | GraphVizGif
  | GraphVizJpeg
  | GraphVizPdf
  | GraphVizPng
  | GraphVizPs
  | GraphVizPs2
  | GraphVizSvg
  | GraphVizSvgZ
  | GraphVizTiff
  | GraphVizWebP
  deriving (Eq, Show, Bounded, Enum)

data Opts f = Opts
  { optOutputFormat :: f OutputFormat
  }

type OptsPartial = Opts Maybe

type OptsTotal = Opts Identity

defaultOpts :: OptsTotal
defaultOpts =
  Opts
    { optOutputFormat = pure GraphVizPdf
    }

fillMissingOpts :: OptsPartial -> OptsTotal
fillMissingOpts op =
  Opts
    { optOutputFormat = fill optOutputFormat
    }
  where
    fill :: (forall g. Opts g -> g a) -> Identity a
    fill f = maybe (f defaultOpts) pure $ f op

optsParserInfo :: ParserInfo OptsTotal
optsParserInfo = info optsParser fullDesc

optsParser :: Parser OptsTotal
optsParser =
  fillMissingOpts . Opts
    <$> (optional . option parseBoundedEnum . mconcat)
      [ short 'f',
        long "output-format",
        metavar "FORMAT",
        help $
          "Output format: "
            <> showAllValues @OutputFormat
            <> showDefaultValue optOutputFormat
      ]

parseBoundedEnum ::
  forall a.
  (Enum a, Bounded a, ToCLIArgument a) =>
  ReadM a
parseBoundedEnum =
  eitherReader
    ( \s ->
        case lookup s argumentToValue of
          Just v -> Right v
          Nothing ->
            Left $
              "unknown value: '"
                <> s
                <> "'\nValid values are: "
                <> showAllValues @a
                <> "."
    )
  where
    argumentToValue = map (\x -> (toCLIArgument x, x)) [minBound ..]

showAllValues :: forall a. (Enum a, Bounded a, ToCLIArgument a) => String
showAllValues = intercalate ", " (toCLIArgument <$> [(minBound :: a) ..])

-- | CLI representation of the default value of an option, formatted for
-- inclusion in the help text.
showDefaultValue ::
  ToCLIArgument a =>
  (OptsTotal -> Identity a) ->
  String
showDefaultValue =
  (" (default " <>)
    . (<> ")")
    . toCLIArgument
    . runIdentity
    . ($ defaultOpts)

class ToCLIArgument a where
  toCLIArgument :: a -> String

instance ToCLIArgument OutputFormat where
  toCLIArgument SQL = "sql"
  toCLIArgument Morela = "mrl"
  toCLIArgument GraphVizDotOutput = "dot"
  toCLIArgument GraphVizEps = "eps"
  toCLIArgument GraphVizFig = "fig"
  toCLIArgument GraphVizGif = "gif"
  toCLIArgument GraphVizJpeg = "jpeg"
  toCLIArgument GraphVizPdf = "pdf"
  toCLIArgument GraphVizPng = "png"
  toCLIArgument GraphVizPs = "ps"
  toCLIArgument GraphVizPs2 = "ps2"
  toCLIArgument GraphVizSvg = "svg"
  toCLIArgument GraphVizSvgZ = "svgz"
  toCLIArgument GraphVizTiff = "tiff"
  toCLIArgument GraphVizWebP = "webp"

partialToGraphvizOutput :: OutputFormat -> GC.GraphvizOutput
partialToGraphvizOutput GraphVizDotOutput = GC.DotOutput
partialToGraphvizOutput GraphVizEps = GC.Eps
partialToGraphvizOutput GraphVizFig = GC.Fig
partialToGraphvizOutput GraphVizGif = GC.Gif
partialToGraphvizOutput GraphVizJpeg = GC.Jpeg
partialToGraphvizOutput GraphVizPdf = GC.Pdf
partialToGraphvizOutput GraphVizPng = GC.Png
partialToGraphvizOutput GraphVizPs = GC.Ps
partialToGraphvizOutput GraphVizPs2 = GC.Ps2
partialToGraphvizOutput GraphVizSvg = GC.Svg
partialToGraphvizOutput GraphVizSvgZ = GC.SvgZ
partialToGraphvizOutput GraphVizTiff = GC.Tiff
partialToGraphvizOutput GraphVizWebP = GC.WebP
partialToGraphvizOutput SQL = undefined
partialToGraphvizOutput Morela = undefined
