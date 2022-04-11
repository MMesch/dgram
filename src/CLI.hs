module CLI where

import Data.List (intercalate)
import Data.Semigroup ((<>))
import Lib
import Options.Applicative
import Prelude
import Types

cli :: IO Command
cli =
  execParser
    ( info
        (opts <**> helper)
        ( fullDesc
            <> progDesc "A diagram file to image converter"
            <> header "convert diagrams to images"
        )
    )

opts :: Parser Command
opts =
  subparser $
    command
      "convert"
      ( info
          (ConvertCommand <$> convertOptionParser <**> helper)
          ( fullDesc
              <> progDesc "Convert diagram files to images"
              <> header "convert diagrams to images"
          )
      )
      <> command
        "init"
        ( info
            (InitCommand <$> initOptionParser <**> helper)
            ( fullDesc
                <> progDesc "initialize an example file from library"
                <> header "init diagram file from library"
            )
        )

convertOptionParser :: Parser ConvertOptions
convertOptionParser =
  ConvertOptions
    <$> optional
      ( option
          auto
          ( long "format"
              <> metavar "FORMAT"
              <> help
                ( "The format of the output file. One of: "
                    <> intercalate ", " (show <$> enumFrom SVG)
                )
          )
      )
    <*> optional
      ( option
          auto
          ( long "runner"
              <> metavar "RUNNER"
              <> help
                ( "The program that is used for conversion. One of: "
                    <> intercalate ", " (show <$> enumFrom VegaLite)
                )
          )
      )
    <*> argument
      str
      ( metavar "infile"
          <> help "The file path of the input file"
      )
    <*> argument
      str
      ( metavar "outfile"
          <> help "The file path of the output file"
      )
    <*> strOption
      ( long "extraOptions"
          <> showDefault
          <> value ""
          <> metavar "OptionsString"
          <> help "extraoptions that will be passed to the executable"
      )

initOptionParser :: Parser InitOptions
initOptionParser =
  InitOptions
    <$> argument
      str
      ( metavar "ExampleName"
          <> help "one of"
      )
    <*> argument
      str
      ( metavar "PATH"
          <> help "output file name (or directory)"
      )
