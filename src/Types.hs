module Types where

import Prelude

data Command = ConvertCommand ConvertOptions | InitCommand InitOptions

data Format = SVG | PDF | PNG deriving (Read, Show, Enum)

data Runner = VegaLite | Vega | GraphViz | Mermaid | Svgbob | Plantuml
  deriving (Read, Show, Enum)

data InitOptions = InitOptions
  { example :: String,
    path :: FilePath
  }
  deriving (Show)

data ConvertOptions = ConvertOptions
  { maybeFormat :: Maybe Format,
    maybeResolution :: Maybe Int,
    maybeRunner :: Maybe Runner,
    infile :: FilePath,
    outfile :: FilePath,
    extraOptions :: String
  }
  deriving (Show)
