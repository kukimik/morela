{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import qualified Data.ByteString as SB
import Data.Functor.Identity (runIdentity)
import Data.GraphViz.Commands as GC
import qualified Data.Text.Lazy.IO as LTIO
import Morela.Config
import Morela.Parse (toDiagram)
import Morela.Render (diagramToDotGraph)
import Morela.Writer.SQL (diagramToSQL)
import Options.Applicative (execParser)
import System.Exit (die)
import System.IO (stdin, stdout)
import Text.Parsec (parse)
import Text.Parsec.Morela.Parser (document)

main :: IO ()
main = do
  GC.quitWithoutGraphviz "GraphViz is not installed on your system."
  opts <- execParser optsParserInfo
  input <- LTIO.hGetContents stdin
  case first show (parse document "" input) >>= toDiagram of
    Left err -> die err
    Right diag ->
      case runIdentity $ optOutputFormat opts of
        MorelaOutputFormat SQL ->
          LTIO.hPutStr stdout (diagramToSQL diag)
        MorelaOutputFormat Morela ->
          die "Not implemented yet."
        GraphvizOutputFormat fmt ->
          GC.graphvizWithHandle
            GC.Dot
            (diagramToDotGraph diag)
            (toGraphvizOutput fmt)
            (SB.hGetContents >=> SB.hPut stdout)
