import Prelude
import CLI
import Lib
import Types

main :: IO ()
main = do
  cmd <- topLevelCLI
  case cmd of
    ConvertCommand convertOptions -> convertWith convertOptions
    TemplateCommand templateOptions -> error "not implemented yet"
