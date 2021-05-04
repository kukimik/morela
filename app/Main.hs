{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import qualified Data.ByteString as SB
import Data.Functor.Identity (runIdentity)
import Data.GraphViz.Commands as GC
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (fromStrict)
import Morela.Config (Opts (..), OutputFormat (..), optsParserInfo, partialToGraphvizOutput)
import Morela.Parse (toDiagram)
import Morela.Render (diagramToDotGraph)
import Options.Applicative (execParser)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import Text.Parsec
import Text.Parsec.Morela.Parser (document)

main :: IO ()
main = do
  GC.quitWithoutGraphviz "GraphViz is not installed on your system."
  opts <- execParser optsParserInfo
  input <- SB.hGetContents stdin
  case first
    show
    (parse document "" (fromStrict $ decodeUtf8 input))
    >>= toDiagram of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right diag ->
      case runIdentity $ optOutputFormat opts of
        SQL ->
          undefined
        Morela ->
          undefined
        fmt ->
          GC.graphvizWithHandle
            GC.Dot
            (diagramToDotGraph diag)
            (partialToGraphvizOutput fmt)
            (SB.hGetContents >=> SB.hPut stdout)
