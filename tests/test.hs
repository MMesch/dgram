import Data.List
import Data.Ord
import Lib
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import qualified System.IO.Strict as S
import Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L
import Replace.Megaparsec (streamEdit)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [goldenTests]

goldenTests :: TestTree
goldenTests =
  testGroup
    "Golden tests"
    [vegaLiteTest,
     vegaTest,
     graphvizTest,
     mermaidTest,
     svgbobTest,
     plantumlTest]

convertTest :: FilePath -> FilePath -> IO ()
convertTest infile outfile = 
          convertWith
            ConvertOptions
              { maybeFormat = Nothing,
                maybeRunner = Nothing,
                maybeResolution = Nothing,
                infile = infile,
                outfile = outfile,
                extraOptions = ""
              }

convertTestWithFixUp :: (FilePath -> IO ()) -> FilePath -> FilePath -> IO ()
convertTestWithFixUp fixup infile outfile = do
        convertTest infile outfile
        fixup outfile

vegaTest =
  let infile = "./examples/vega.vg"
      goldenfile = "./examples/vega.svg"
      outfile = "tests/output/vega.svg"
  in goldenVsFile "test vega example" goldenfile outfile
      (convertTest infile outfile)

vegaLiteTest =
  let infile = "./examples/vegalite.vl"
      goldenfile = "./examples/vegalite.svg"
      outfile = "tests/output/vegalite.svg"
  in goldenVsFile "test vega lite example" goldenfile outfile
      (convertTest infile outfile)

graphvizTest =
  let infile = "./examples/graphviz.dot"
      goldenfile = "./examples/graphviz.svg"
      outfile = "tests/output/graphviz.svg"
  in goldenVsFile "test graphviz example" goldenfile outfile
      (convertTest infile outfile)

getSvgIdParser :: Parsec Void String String
getSvgIdParser =
        chunk "<svg id=\"" *> manyTill L.charLiteral (chunk "\"")

mermaidTest =
  let infile = "./examples/mermaid.mmd"
      goldenfile = "./examples/mermaid.svg"
      outfile = "./tests/output/mermaid.svg"
      fixup fp = do
        print "fixing up svg id"
        str <- S.readFile fp
        let eitherId = runParser getSvgIdParser outfile str
        case eitherId of
          Left err -> print $ "Can't equalize mermaid id" <> show err
          Right svgId -> do
            print $ "found id " <> svgId
            let pattern = chunk svgId :: Parsec Void String String
                newstr = streamEdit pattern (const "equalizedId") str
            writeFile fp newstr
  in goldenVsFile "test mermaid example" goldenfile outfile
      (convertTestWithFixUp fixup infile outfile)

svgbobTest =
  let infile = "./examples/svgbob.bob"
      goldenfile = "./examples/svgbob.svg"
      outfile = "tests/output/svgbob.svg"
  in goldenVsFile "test svgbob example" goldenfile outfile
      (convertTest infile outfile)

plantumlTest =
  let infile = "./examples/plantuml.puml"
      goldenfile = "./examples/plantuml.svg"
      outfile = "tests/output/plantuml.svg"
  in goldenVsFile "test plantuml example" goldenfile outfile
      (convertTest infile outfile)
