{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import qualified Data.ByteString as SB
import Data.GraphViz.Commands
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (fromStrict)
import Morela.Parse (toDiagram)
import Morela.Render (diagramToDotGraph)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import Text.Parsec
import Text.Parsec.Morela.Parser (document)

main :: IO ()
main = do
  checkRequirements -- application may terminate here
  input <- SB.hGetContents stdin
  case first
    show
    (parse document "" (fromStrict $ decodeUtf8 input))
    >>= toDiagram of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right diag ->
      graphvizWithHandle
        Dot
        (diagramToDotGraph diag)
        Pdf
        (SB.hGetContents >=> SB.hPut stdout)

checkRequirements :: IO ()
checkRequirements = quitWithoutGraphviz msg
  where
    msg =
      "GraphViz is not installed on your system.\n"
        ++ "Please install it first. https://github.com/kukimik/morela"
