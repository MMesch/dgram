{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Data.Maybe (fromMaybe)
import Data.Text
import System.FilePath (takeExtension)
import System.Process
import Prelude

{-
   This module contains the converter datatype and associated functions.
   A converter is an executable (configured to be available on path), with
   possible theme/configuration. It can be invoked with a set of common options.
   The actual runners are configured in a YAML file for easy contributions.
-}

data Format = SVG | PDF | PNG deriving (Read, Show, Enum)

data RunnerType = VegaLite | Vega | GraphViz deriving (Read, Show, Enum)

data Task = Task
  { format :: Format,
    runnerType :: Maybe RunnerType,
    infile :: FilePath,
    outfile :: FilePath,
    extraOptions :: Text
  }
  deriving (Show)

data Runner = Runner
  { runnerName :: RunnerType,
    description :: Text,
    extensions :: [Text],
    standardOptions :: Text
  }
  deriving (Show)

convertWith :: Task -> IO ()
convertWith Task {..} = do
  let runner = case runnerType of
        Just x -> x
        Nothing -> case takeExtension infile of
          ".vl" -> VegaLite
          ".vg" -> Vega
          ".dot" -> GraphViz
          _ -> VegaLite
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
  (ecode, stdout, stderr) <-
    readProcessWithExitCode exec args ""
  print (ecode, stdout, stderr)
