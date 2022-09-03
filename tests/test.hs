{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.List
import Data.Ord
import Data.Void
import Data.Word (Word8)
import Lib
import Replace.Megaparsec (streamEdit)
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
    [ vegaLiteTest,
      vegaTestGenerator ".svg",
      vegaTestGenerator ".pdf",
      graphvizTest,
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

vegaTestGenerator :: String -> TestTree
vegaTestGenerator extension =
  let infile = "./examples/vega.vg"
      goldenfile = "./examples/vega" <> extension
      outfile = "tests/output/vega" <> extension

      -- fixup pdf dates
      fixup = replaceFixup outfile "/CreationDate (D:" ")" "20220830151034+02'00"

   in goldenVsFile "test vega example" goldenfile outfile $
        if extension == ".pdf"
          then convertTestWithFixUp fixup infile outfile
          else convertTest infile outfile

vegaLiteTest =
  let infile = "./examples/vegalite.vl"
      goldenfile = "./examples/vegalite.svg"
      outfile = "tests/output/vegalite.svg"
   in goldenVsFile
        "test vega lite example"
        goldenfile
        outfile
        (convertTest infile outfile)

graphvizTest =
  let infile = "./examples/graphviz.dot"
      goldenfile = "./examples/graphviz.svg"
      outfile = "tests/output/graphviz.svg"
   in goldenVsFile
        "test graphviz example"
        goldenfile
        outfile
        (convertTest infile outfile)

mermaidTest =
  let infile = "./examples/mermaid.mmd"
      goldenfile = "./examples/mermaid.svg"
      outfile = "./tests/output/mermaid.svg"

      -- ids are generated randomly and thus need to be equalized before
      -- comparison
      fixup = replaceFixup outfile "<svg id=\"" "\"" "equalizedId"
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
  FilePath ->
  BS.ByteString ->
  BS.ByteString ->
  BS.ByteString ->
  FilePath ->
  IO ()
replaceFixup outfile before after replacement fp = do
  print "fixing up non-reproducible elements of file"
  str <- BS.readFile fp
  let
    parser = betweenParser before after
    eitherId = runParser parser outfile str
  case eitherId of
    Left err -> print $ "Can't equalize mermaid id" <> show err
    Right match -> do
      print $ "found id " <> match
      let pattern = chunk match :: Parsec Void BS.ByteString BS.ByteString
          newstr = streamEdit pattern (const replacement) str
      BS.writeFile fp newstr

betweenParser :: BS.ByteString -> BS.ByteString -> Parsec Void BS.ByteString BS.ByteString
betweenParser before after = do
  _ <- manyTill anySingle (chunk before)
  ret <- manyTill anySingle (chunk after)
  return $ BS.pack ret
