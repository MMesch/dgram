module CLI where

import Data.List (intercalate, words)
import Data.Semigroup ((<>))
import Lib
import Options.Applicative
import Types
import Prelude

topLevelCLI :: IO Command
topLevelCLI =
  execParser
    ( info
        (topLevelCommands <**> helper)
        ( fullDesc
            <> progDesc "A diagram file to image converter"
            <> header "convert diagrams to images"
        )
    )

topLevelCommands :: Parser Command
topLevelCommands =
  subparser $ command "convert" convertDesc <> command "init" templateDesc
  where
    convertDesc =
      info
        (ConvertCommand <$> convertCommandParser <**> helper)
        ( fullDesc
            <> progDesc "Convert diagram files to images"
            <> header "convert diagrams to images"
        )
    templateDesc =
      info
        (TemplateCommand <$> initCommandParser <**> helper)
        ( fullDesc
            <> progDesc "initialize an example file from library"
            <> header "init diagram file from library"
        )

convertCommandParser :: Parser ConvertOptions
convertCommandParser =
  ConvertOptions
    <$> optional
      ( option
          auto
          ( long "informat"
              <> metavar "INFORMAT"
              <> help
                ( "The informat, specifying the program that is used for conversion. One of: "
                    <> intercalate ", " (show <$> (allValues :: [InFormat]))
                )
          )
      )
    <*> optional
      ( option
          auto
          ( long "outformat"
              <> metavar "OUTFORMAT"
              <> help
                ( "The format of the output file. One of: "
                    <> intercalate ", " (show <$> (allValues :: [OutFormat]))
                )
          )
      )
    <*> argument
      str
      ( metavar "INPATH"
          <> help "The file path of the input file"
      )
    <*> optional
      ( option
          auto
          ( long "outpath" <> metavar "OUTPATH"
              <> help "The file path of the output file"
          )
      )
    <*> option readExtraOptions 
      ( long "extraOptions"
          <> showDefault
          <> value []
          <> metavar "OptionsString"
          <> help "extraoptions that will be passed to the executable. E.g. \"-s\""
      )

readExtraOptions :: ReadM [String]
readExtraOptions = eitherReader $ \s -> Right $ words s 

initCommandParser :: Parser TemplateOptions
initCommandParser =
  TemplateOptions
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
