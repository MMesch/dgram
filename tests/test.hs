{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord
import Lib
import Test.Tasty.Golden

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [goldenTests]

goldenTests = testGroup "Golden tests"
  [ let infile = "./tests/data/vegalite.vl"
        goldenfile = "./tests/golden/vegalite.svg"
        outfile = "tests/output/vegalite.svg"
    in
        goldenVsFile "test vega lite example" goldenfile outfile $
          convertWith Task {format=SVG, runnerType=Nothing, infile=infile,
                 outfile=outfile, extraOptions=""}
  ]
