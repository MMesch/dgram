{-# LANGUAGE OverloadedStrings #-}

import Prelude
import CLI
import Convert
import Types

main :: IO ()
main = do
  cmd <- cli
  case cmd of
    ConvertCommand convertOptions -> convertWith convertOptions
    InitCommand initOptions -> print "not implemented yet"
