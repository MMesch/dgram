{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import System.Directory
import System.FilePath (dropExtension, joinPath, takeBaseName, takeExtension)
import System.IO.Temp
import System.IO (hPutStrLn, stderr)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process
import Types
import Prelude

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
      args = [inPath co, outPath] ++ extraOptions co
  (ecode, stdout, stderr) <-
    readProcessWithExitCode exec args ""
  printErrorOrSuccess (ecode, stdout, stderr)

vegaConverter :: ConvertOptions -> IO ()
vegaConverter co = do
  let outFormat = guessOutFormat co
      outPath = guessOutPath co
      exec = case outFormat of
        SVG -> "vg2svg"
        PDF -> "vg2pdf"
        PNG -> "vg2png"
      args = [inPath co, outPath] ++ extraOptions co
  print $ "outFormat" <> show outFormat <> " outPath" <> show outPath
  (ecode, stdout, stderr) <-
    readProcessWithExitCode exec args ""
  printErrorOrSuccess (ecode, stdout, stderr)

graphvizConverter :: ConvertOptions -> IO ()
graphvizConverter co = do
  let outFormat = guessOutFormat co
      outPath = guessOutPath co
      formatOption = case outFormat of
        SVG -> "-Tsvg"
        PDF -> "-Tpdf"
        PNG -> "-Tpng"
      exec = "dot"
      args = [inPath co, formatOption, "-o" <> outPath] ++ extraOptions co
  print $ "running graphviz with these arguments: " <> show args
  (ecode, stdout, stderr) <-
    readProcessWithExitCode exec args ""
  printErrorOrSuccess (ecode, stdout, stderr)

mermaidConverter :: ConvertOptions -> IO ()
mermaidConverter co = do
  withSystemTempDirectory
    "mermaid"
    ( \dir -> do
        let outFormat = guessOutFormat co
            outPath = guessOutPath co
            intermediatePath = joinPath [dir, "mermaid.svg"]
            configPath = joinPath [dir, "config.json"]
            exec = "mmdc"
            configStr = "{\"flowchart\": {\"useMaxWidth\": false,\"htmlLabels\": false}}"
            args = ["-i" <> inPath co, "-o" <> intermediatePath, "-c" <> configPath] ++ extraOptions co
            convertExec = "rsvg-convert"
            convertArgs = [intermediatePath, "-f", show outFormat, "-o", outPath]
        writeFile configPath configStr
        (ecode, stdout, stderr) <- readProcessWithExitCode exec args ""
        print (ecode, stdout, stderr)
        case outFormat of
          SVG -> renamePath intermediatePath outPath
          _ -> do
            print $ "converting to " <> outPath
            (ecode, stdout, stderr) <- readProcessWithExitCode convertExec convertArgs ""
            printErrorOrSuccess (ecode, stdout, stderr)
    )

svgbobConverter :: ConvertOptions -> IO ()
svgbobConverter co =
  withSystemTempFile
    ("svgbob" <> toExtension SVG)
    ( \fp _ -> do
        let outFormat = guessOutFormat co
            outPath = guessOutPath co
            exec = "svgbob"
            args = [inPath co, "-o" <> fp] ++ extraOptions co
            convertExec = "rsvg-convert"
            convertArgs = [fp, "-f", show outFormat, "-o", outPath]
        print $ "producing temporary file " <> fp
        (ecode, stdout, stderr) <-
          readProcessWithExitCode exec args ""
        print (ecode, stdout, stderr)
        print $ "converting to " <> outPath
        (ecode, stdout, stderr) <-
          readProcessWithExitCode convertExec convertArgs ""
        printErrorOrSuccess (ecode, stdout, stderr)
    )

plantumlConverter :: ConvertOptions -> IO ()
plantumlConverter co =
  withSystemTempDirectory "plantuml" $ \dirname -> do
    let outFormat = guessOutFormat co
        outPath = guessOutPath co
        formatStr = case outFormat of
          SVG -> "svg"
          PDF -> "pdf"
          PNG -> "png"
        exec = "plantuml"
        inPathBase = takeBaseName (inPath co) <> ".svg"
        outPathPlantuml = joinPath [dirname, inPathBase]
        args = [inPath co, "-o" <> dirname, "-tsvg"] ++ extraOptions co
        convertExec = "rsvg-convert"
        convertArgs = [outPathPlantuml, "-f", show outFormat, "-o", outPath]
    (ecode, stdout, stderr) <-
      readProcessWithExitCode exec args ""
    print (ecode, stderr, stdout)
    case outFormat of
      SVG -> renamePath outPathPlantuml outPath
      _ -> do
        print $ "converting to " <> outPath
        (ecode, stdout, stderr) <-
          readProcessWithExitCode convertExec convertArgs ""
        printErrorOrSuccess (ecode, stdout, stderr)

printErrorOrSuccess :: (ExitCode, String, String) -> IO ()
printErrorOrSuccess (ecode, log, err) = case ecode of
    ExitSuccess -> print "succesful conversion"
    ExitFailure a -> hPutStrLn stderr $ unlines
            ["graphviz exited with error code: " <> show a,
             "here is some log output ...",
             log,
             "and here the error message",
             err]
