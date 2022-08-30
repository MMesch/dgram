module Types where

import Prelude

data Command
  = ConvertCommand ConvertOptions
  | TemplateCommand TemplateOptions

-- template command
data TemplateOptions = TemplateOptions
  { exampleName :: String,
    path :: FilePath
  }
  deriving (Show)

-- convert command
data OutFormat = SVG | PDF | PNG deriving (Read, Show, Enum)

data InFormat = VegaLite | Vega | GraphViz | Mermaid | Svgbob | Plantuml
  deriving (Read, Show, Enum)

data ConvertOptions = ConvertOptions
  { maybeInFormat :: Maybe InFormat,
    maybeOutFormat :: Maybe OutFormat,
    inPath :: FilePath,
    maybeOutPath :: Maybe FilePath,
    extraOptions :: String
  }
  deriving (Show)
