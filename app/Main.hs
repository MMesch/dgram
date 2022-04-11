{-# LANGUAGE OverloadedStrings #-}

import Prelude
import CLI
import Lib

main :: IO ()
main = do
  cmd <- cli
  case cmd of
    ConvertCommand convertOptions -> convertWith convertOptions
    InitCommand initOptions -> print "not implemented yet"
