module Types where

import Prelude

class HasExtension a where
  toExtension :: a -> String
  fromExtension :: String -> a

data Command
  = ConvertCommand ConvertOptions
  | TemplateCommand TemplateOptions

-- template command
data TemplateOptions = TemplateOptions
  { exampleName :: String,
    path :: FilePath
  }
  deriving (Show)

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]

-- convert command
data OutFormat = SVG | PDF | PNG deriving (Read, Show, Enum, Bounded)

instance HasExtension OutFormat where
  toExtension a = case a of
    SVG -> ".svg"
    PDF -> ".pdf"
    PNG -> ".png"
  fromExtension a = case a of
    ".svg" -> SVG
    ".pdf" -> PDF
    ".png" -> PNG
    _ -> error "unknown output format"

allOutExtensions :: [String]
allOutExtensions = toExtension <$> (allValues :: [OutFormat])

data InFormat = VegaLite | Vega | GraphViz | Mermaid | Svgbob | Plantuml
  deriving (Read, Show, Enum, Bounded)

instance HasExtension InFormat where
  toExtension a = case a of
    VegaLite -> ".vl"
    Vega -> ".vg"
    GraphViz -> ".dot"
    Mermaid -> ".mmd"
    Plantuml -> ".puml"
    Svgbob -> ".bob"
  fromExtension a = case a of
    ".vl" -> VegaLite
    ".vg" -> Vega
    ".dot" -> GraphViz
    ".mmd" -> Mermaid
    ".puml" -> Plantuml
    ".bob" -> Svgbob
    _ -> error "unknown input format"

allInExtensions :: [String]
allInExtensions = toExtension <$> (allValues :: [InFormat])

data ConvertOptions = ConvertOptions
  { maybeInFormat :: Maybe InFormat,
    maybeOutFormat :: Maybe OutFormat,
    inPath :: FilePath,
    maybeOutPath :: Maybe FilePath,
    extraOptions :: String
  }
  deriving (Show)
