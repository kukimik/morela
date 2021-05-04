{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Morela.Config
  ( Opts (..),
    OutputFormat (..),
    MorelaOutput (..),
    GraphvizOutput (..),
    optsParserInfo,
    toGraphvizOutput,
  )
where

import Data.Char (toLower)
import Data.Functor.Identity (Identity (..))
import qualified Data.GraphViz.Commands as GC
import Data.List (intercalate)
import Options.Applicative

data GraphvizOutput
  = Dot
  | Eps
  | Fig
  | Gif
  | Jpeg
  | Pdf
  | Png
  | Ps
  | Ps2
  | Svg
  | SvgZ
  | Tiff
  | WebP
  deriving (Eq, Show, Bounded, Enum)

data MorelaOutput
  = SQL
  | Morela
  deriving (Eq, Show, Bounded, Enum)

data OutputFormat
  = MorelaOutputFormat MorelaOutput
  | GraphvizOutputFormat GraphvizOutput
  deriving (Eq, Show)

instance Bounded OutputFormat where
  minBound = MorelaOutputFormat minBound
  maxBound = GraphvizOutputFormat maxBound

instance Enum OutputFormat where
  fromEnum (MorelaOutputFormat fmt) = fromEnum fmt
  fromEnum (GraphvizOutputFormat fmt) = morelaOutputMax + 1 + fromEnum fmt
    where
      morelaOutputMax = fromEnum $ maxBound @MorelaOutput
  toEnum n
    | n <= morelaOutputMax =
      MorelaOutputFormat $ toEnum n
    | otherwise =
      GraphvizOutputFormat $ toEnum (n -1 - morelaOutputMax)
    where
      morelaOutputMax = fromEnum $ maxBound @MorelaOutput

data Opts f = Opts
  { optOutputFormat :: f OutputFormat
  }

type OptsPartial = Opts Maybe

type OptsTotal = Opts Identity

defaultOpts :: OptsTotal
defaultOpts =
  Opts
    { optOutputFormat = pure $ GraphvizOutputFormat Pdf
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
  toCLIArgument (GraphvizOutputFormat fmt) = toLower <$> show fmt
  toCLIArgument (MorelaOutputFormat fmt) = toLower <$> show fmt

toGraphvizOutput :: GraphvizOutput -> GC.GraphvizOutput
toGraphvizOutput Dot = GC.DotOutput
toGraphvizOutput Eps = GC.Eps
toGraphvizOutput Fig = GC.Fig
toGraphvizOutput Gif = GC.Gif
toGraphvizOutput Jpeg = GC.Jpeg
toGraphvizOutput Pdf = GC.Pdf
toGraphvizOutput Png = GC.Png
toGraphvizOutput Ps = GC.Ps
toGraphvizOutput Ps2 = GC.Ps2
toGraphvizOutput Svg = GC.Svg
toGraphvizOutput SvgZ = GC.SvgZ
toGraphvizOutput Tiff = GC.Tiff
toGraphvizOutput WebP = GC.WebP
