{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Maybe (fromMaybe)
import System.Directory
import System.FilePath (joinPath, takeBaseName, takeExtension, dropExtension)
import System.IO.Temp
import System.Process
import Types
import Prelude
import Debug.Trace(trace)

convertWith :: ConvertOptions -> IO ()
convertWith co = do
  case guessInFormat co of
    VegaLite -> vegaliteConverter co
    Vega -> vegaConverter co
    GraphViz -> graphvizConverter co
    Mermaid -> mermaidConverter co
    Svgbob -> svgbobConverter co
    Plantuml -> plantumlConverter co

guessInFormat :: ConvertOptions -> InFormat
guessInFormat ConvertOptions {maybeInFormat, inPath} =
  case maybeInFormat of
    Just x -> x
    Nothing -> fromExtension $ takeExtension inPath

guessOutFormat :: ConvertOptions -> OutFormat
guessOutFormat co@ConvertOptions {maybeOutPath, maybeOutFormat} =
  case maybeOutFormat of
    Just format -> format
    Nothing -> fromExtension $ takeExtension (guessOutPath co)

guessOutPath :: ConvertOptions -> FilePath
guessOutPath ConvertOptions {maybeOutPath, inPath} =
  case maybeOutPath of
    Just path -> path
    Nothing -> dropExtension inPath <> ".svg"

vegaliteConverter :: ConvertOptions -> IO ()
vegaliteConverter co = do
  let outFormat = guessOutFormat co
      outPath = guessOutPath co
      exec = case outFormat of
        SVG -> "vl2svg"
        PDF -> "vl2pdf"
        PNG -> "vl2png"
      args = [inPath co, outPath, extraOptions co]
  (ecode, stdout, stderr) <-
    readProcessWithExitCode exec args ""
  print (ecode, stdout, stderr)

vegaConverter :: ConvertOptions -> IO ()
vegaConverter co = do
  let outFormat = guessOutFormat co
      outPath = guessOutPath co
      exec = case outFormat of
        SVG -> "vg2svg"
        PDF -> "vg2pdf"
        PNG -> "vg2png"
      args = [inPath co, outPath]
  print $ "outFormat" <> show outFormat <> " outPath" <> show outPath
  (ecode, stdout, stderr) <-
    readProcessWithExitCode exec args ""
  print (ecode, stdout, stderr)

graphvizConverter :: ConvertOptions -> IO ()
graphvizConverter co = do
      let
          outFormat = guessOutFormat co
          outPath = guessOutPath co
          formatOption = case outFormat of
            SVG -> "-Tsvg"
            PDF -> "-Tpdf"
            PNG -> "-Tpng"
          exec = "dot"
          args = [formatOption, inPath co, "-o" <> outPath, extraOptions co]
      (ecode, stdout, stderr) <-
        readProcessWithExitCode exec args ""
      print (ecode, stdout, stderr)

mermaidConverter :: ConvertOptions -> IO ()
mermaidConverter co = do
      let
          outFormat = guessOutFormat co
          outPath = guessOutPath co
          exec = "mmdc"
          args = ["-i" <> inPath co, "-o" <> outPath, extraOptions co]
      (ecode, stdout, stderr) <-
        readProcessWithExitCode exec args ""
      print (ecode, stdout, stderr)

svgbobConverter :: ConvertOptions -> IO ()
svgbobConverter co = do
      let
          outFormat = guessOutFormat co
          outPath = guessOutPath co
          exec = "svgbob"
          args = [inPath co, "-o" <> outPath, extraOptions co]
      (ecode, stdout, stderr) <-
        readProcessWithExitCode exec args ""
      print (ecode, stdout, stderr)

plantumlConverter :: ConvertOptions -> IO ()
plantumlConverter co =
      withSystemTempDirectory "plantuml" $ \dirname -> do
        let
            outFormat = guessOutFormat co
            outPath = guessOutPath co
            formatStr = case outFormat of
              SVG -> "svg"
              PDF -> "pdf"
              PNG -> "png"
            exec = "plantuml"
            inPathBase = takeBaseName (inPath co) <> "." <> formatStr
            outPathPlantuml = joinPath [dirname, inPathBase]
            args = [inPath co, "-o" <> dirname, "-t" <> formatStr, extraOptions co]
        (ecode, stdout, stderr) <-
          readProcessWithExitCode exec args ""
        print (ecode, stderr)
        putStr stdout
        renamePath outPathPlantuml outPath
