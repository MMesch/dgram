{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Ord
import Lib
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [goldenTests]

goldenTests :: TestTree
goldenTests =
  testGroup
    "Golden tests"
    [ let infile = "./examples/vegalite.vl"
          goldenfile = "./examples/vegalite.svg"
          outfile = "tests/output/vegalite.svg"
       in goldenVsFile "test vega lite example" goldenfile outfile $
            convertWith
              Task
                { maybeFormat = Nothing,
                  maybeRunner = Nothing,
                  infile = infile,
                  outfile = outfile,
                  extraOptions = ""
                }
    ]
