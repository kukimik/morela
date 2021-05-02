{-# LANGUAGE OverloadedStrings #-}

module Main
  (main)
where

import Data.Bifunctor(first)
import qualified Data.ByteString                     as SB
import           Data.GraphViz.Commands
import           System.Exit                         (exitFailure)
import           System.IO                           (hPutStrLn, stderr,stdin,stdout)
import Data.Text.Encoding(decodeUtf8)
import           Text.Parsec.Morela.Parser(document)
import           Morela.Render(diagramToDotGraph)
import           Morela.Parse(toDiagram)
import Text.Parsec
import Data.Text.Lazy (fromStrict)

main :: IO ()
main = do
  checkRequirements -- application may terminate here
  input <- SB.hGetContents stdin
  case (first show $ parse document "" (fromStrict $ decodeUtf8 input)) >>= toDiagram of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right diag -> do
      graphvizWithHandle Dot (diagramToDotGraph diag) Pdf (\h -> SB.hGetContents h >>= SB.hPut stdout) -- customize command (Dot vs Neato)

checkRequirements :: IO ()
checkRequirements = quitWithoutGraphviz msg
  where
    msg = "GraphViz is not installed on your system.\n" ++
          "Please install it first. https://github.com/kukimik/morela"
