import Data.List
import Data.Ord
import Lib
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Types

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [goldenTests]

goldenTests :: TestTree
goldenTests =
  testGroup
    "Golden tests"
    [vegaLiteTest,
     graphvizTest]

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

vegaLiteTest =
  let infile = "./examples/vegalite.vl"
      goldenfile = "./examples/vegalite.svg"
      outfile = "tests/output/vegalite.svg"
  in goldenVsFile "test vega lite example" goldenfile outfile
      (convertTest infile outfile)

graphvizTest =
  let infile = "./examples/graphviz.dot"
      goldenfile = "./examples/graphviz.svg"
      outfile = "tests/output/vegalite.svg"
  in goldenVsFile "test graphviz example" goldenfile outfile
      (convertTest infile outfile)
