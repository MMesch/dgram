{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Data.Maybe (fromMaybe)
import System.FilePath (takeExtension)
import System.Process
import Prelude
import Types

convertWith :: ConvertOptions -> IO ()
convertWith ConvertOptions {..} = do
  let runner = case maybeRunner of
        Just x -> x
        Nothing -> case takeExtension infile of
          ".vl" -> VegaLite
          ".vg" -> Vega
          ".dot" -> GraphViz
          ".mmd" -> Mermaid
          ".puml" -> Plantuml
          ".bob" -> Svgbob
          _ -> error "unknown input format"
      format = case maybeFormat of
         Just x -> x
         Nothing -> case takeExtension outfile of
           ".svg" -> SVG
           ".pdf" -> PDF
           ".png" -> PNG
           _ -> error "unknown output format"
  print $ "RUNNER used: " <> show runner
  let (exec, args) = case runner of
        VegaLite ->
          let exec = case format of
                SVG -> "vl2svg"
                PDF -> "vl2pdf"
                PNG -> "vl2png"
              args = [infile, outfile]
           in (exec, args)
        Vega ->
          let executable = case format of
                SVG -> "vg2svg"
                PDF -> "vg2pdf"
                PNG -> "vg2png"
              args = [infile, outfile]
           in (exec, args)
        GraphViz ->
          let formatOption = case format of
                SVG -> "-Tsvg"
                PDF -> "-Tpdf"
                PNG -> "-Tpng"
              exec = "dot"
              args = [formatOption, infile, "-o" <> outfile]
           in (exec, args)
        Mermaid ->
          let
              exec = "mmdc"
              args = ["-i" <> infile, "-o" <> outfile]
           in (exec, args)
        Svgbob ->
          let
              exec = "svgbob"
              args = [infile, "-o" <> outfile]
           in (exec, args)
        Plantuml ->
          let
              exec = "plantuml"
              args = [infile]
           in (exec, args)
  (ecode, stdout, stderr) <-
    readProcessWithExitCode exec args ""
  print (ecode, stdout, stderr)
