{-# LANGUAGE OverloadedStrings #-}

import Data.Semigroup ((<>))
import Lib
import Data.List (intercalate)
import Options.Applicative

cmdOptions :: Parser Task 
cmdOptions =
  Task
    <$> option auto
      ( long "format"
          <> showDefault
          <> value SVG
          <> metavar "FORMAT"
          <> help ("The format of the output file. Possibilities: " 
            <> intercalate ", " (show <$> enumFrom SVG))
      )
    <*> optional (option auto
      ( long "runnerType"
          <> metavar "FORMAT"
          <> help ("The format of the output file. Possibilities: " 
            <> intercalate ", " (show <$> enumFrom VegaLite))
      ))
    <*> argument str (
          metavar "infile"
          <> help "The file path of the input file"
      )
    <*> argument str (
          metavar "outfile"
          <> help "The file path of the output file"
      )
    <*> strOption
      ( long "extraOptions"
          <> showDefault
          <> value ""
          <> metavar "OptionsString"
          <> help "extraoptions that will be passed to the executable"
      )

opts :: ParserInfo Task
opts =
  info
    (cmdOptions <**> helper)
    ( fullDesc
        <> progDesc "Convert diagram files to images"
        <> header "ddgram -- convert diagrams to images"
    )

main :: IO ()
main = do
  cmdOptions <- execParser opts
  convertWith cmdOptions 
  print cmdOptions
