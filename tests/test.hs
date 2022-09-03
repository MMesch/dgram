{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.Char (toLower)
import Data.List
import Data.Ord
import Data.Void
import Data.Word (Word8)
import Lib
import Replace.Megaparsec (streamEdit)
import System.FilePath (takeExtension)
import qualified System.IO.Strict as S
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Text.Megaparsec
import qualified Text.Megaparsec.Byte as MPB
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [goldenTests]

goldenTests :: TestTree
goldenTests =
  testGroup
    "Golden tests"
    [ testGenerator VegaLite,
      testGenerator Vega,
      testGenerator GraphViz,
      mermaidTest,
      svgbobTest,
      plantumlTest
    ]

convertTest :: FilePath -> FilePath -> IO ()
convertTest infile outfile =
  convertWith
    ConvertOptions
      { maybeInFormat = Nothing,
        maybeOutFormat = Nothing,
        inPath = infile,
        maybeOutPath = Just outfile,
        extraOptions = ""
      }

convertTestWithFixUp :: (FilePath -> IO ()) -> FilePath -> FilePath -> IO ()
convertTestWithFixUp fixup infile outfile = do
  convertTest infile outfile
  fixup outfile

testGenerator :: InFormat -> TestTree
testGenerator inFormat =
  let basename = toLower <$> show inFormat
      inPath = "./examples/" <> basename <> toExtension inFormat
      testPaths =
        [ ( "./tests/output/" <> basename <> extension,
            "./examples/" <> basename <> extension
          )
          | extension <- allOutExtensions
        ]
      fixup fp = case fromExtension $ takeExtension fp of
        PDF -> replaceFixup "/CreationDate (D:" ")" "20220830151034+02'00" fp
        SVG -> replaceFixup "<svg id=\"" "\"" "equalizedId" fp
        _ -> return ()
   in testGroup (basename <> " tests") $
        [ goldenVsFile (basename <> " " <> show inFormat <> " test") goldenPath outPath $
            convertTestWithFixUp fixup inPath outPath
          | (outPath, goldenPath) <- testPaths
        ]

mermaidTest =
  let infile = "./examples/mermaid.mmd"
      goldenfile = "./examples/mermaid.svg"
      outfile = "./tests/output/mermaid.svg"

      -- ids are generated randomly and thus need to be equalized before
      -- comparison
      fixup = replaceFixup "<svg id=\"" "\"" "equalizedId"
   in goldenVsFile
        "test mermaid example"
        goldenfile
        outfile
        (convertTestWithFixUp fixup infile outfile)

svgbobTest =
  let infile = "./examples/svgbob.bob"
      goldenfile = "./examples/svgbob.svg"
      outfile = "tests/output/svgbob.svg"
   in goldenVsFile
        "test svgbob example"
        goldenfile
        outfile
        (convertTest infile outfile)

plantumlTest =
  let infile = "./examples/plantuml.puml"
      goldenfile = "./examples/plantuml.svg"
      outfile = "tests/output/plantuml.svg"
   in goldenVsFile
        "test plantuml example"
        goldenfile
        outfile
        (convertTest infile outfile)

-- helpers
replaceFixup ::
  BS.ByteString ->
  BS.ByteString ->
  BS.ByteString ->
  FilePath ->
  IO ()
replaceFixup before after replacement fp = do
  print "fixing up non-reproducible elements of file"
  str <- BS.readFile fp
  let parser = betweenParser before after
      eitherId = runParser parser fp str
  case eitherId of
    Left err -> print $ "Can't equalize " <> show err
    Right match -> do
      print $ "found match: " <> match
      let pattern = chunk match :: Parsec Void BS.ByteString BS.ByteString
          newstr = streamEdit pattern (const replacement) str
      BS.writeFile fp newstr

betweenParser :: BS.ByteString -> BS.ByteString -> Parsec Void BS.ByteString BS.ByteString
betweenParser before after = do
  _ <- manyTill anySingle (chunk before)
  ret <- manyTill anySingle (chunk after)
  return $ BS.pack ret
