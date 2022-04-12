{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Data.Maybe (fromMaybe)
import System.FilePath (takeExtension, takeBaseName, joinPath)
import System.IO.Temp
import System.Process
import System.Directory
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
  case runner of
        VegaLite -> do
          let exec = case format of
                SVG -> "vl2svg"
                PDF -> "vl2pdf"
                PNG -> "vl2png"
              args = [infile, outfile, extraOptions]
          (ecode, stdout, stderr) <-
            readProcessWithExitCode exec args ""
          print (ecode, stdout, stderr)
        Vega -> do
          let exec = case format of
                SVG -> "vg2svg"
                PDF -> "vg2pdf"
                PNG -> "vg2png"
              args = [infile, outfile]
          (ecode, stdout, stderr) <-
            readProcessWithExitCode exec args ""
          print (ecode, stdout, stderr)
        GraphViz -> do
          let formatOption = case format of
                SVG -> "-Tsvg"
                PDF -> "-Tpdf"
                PNG -> "-Tpng"
              exec = "dot"
              args = [formatOption, infile, "-o" <> outfile, extraOptions]
          (ecode, stdout, stderr) <-
            readProcessWithExitCode exec args ""
          print (ecode, stdout, stderr)
        Mermaid -> do
          let
              exec = "mmdc"
              args = ["-i" <> infile, "-o" <> outfile, extraOptions]
          (ecode, stdout, stderr) <-
            readProcessWithExitCode exec args ""
          print (ecode, stdout, stderr)
        Svgbob -> do
          let
              exec = "svgbob"
              args = [infile, "-o" <> outfile, extraOptions]
          (ecode, stdout, stderr) <-
            readProcessWithExitCode exec args ""
          print (ecode, stdout, stderr)
        Plantuml ->
          withSystemTempDirectory "plantuml" $ \dirname -> do
            let
                formatStr = case format of
                  SVG -> "svg"
                  PDF -> "pdf"
                  PNG -> "png"
                exec = "plantuml"
                infileBase = takeBaseName infile <> "." <> formatStr
                outfilePlantuml = joinPath [dirname, infileBase]
                args = [infile, "-o" <> dirname, "-t" <> formatStr, extraOptions]
            (ecode, stdout, stderr) <-
              readProcessWithExitCode exec args ""
            print (ecode, stderr)
            putStr stdout
            renamePath outfilePlantuml outfile
