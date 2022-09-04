{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (toLower)
import Data.Ord
import Data.Void
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Data.Word (Word8)
import Data.Either (rights)
import Control.Monad (forM)
import Control.Applicative.Combinators (choice)
import Lib
import Replace.Megaparsec (streamEditT, sepCap)
import System.FilePath (takeExtension)
import qualified System.IO.Strict as S
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Text.Megaparsec
import qualified Text.Megaparsec.Byte as MPB
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [goldenTests]

goldenTests :: TestTree
goldenTests =
  testGroup
    "Golden tests"
    [ testGenerator VegaLite,
      testGenerator Vega,
      testGenerator GraphViz,
      testGenerator Mermaid,
      svgbobTest,
      plantumlTest
    ]

convertTest :: FilePath -> FilePath -> IO ()
convertTest infile outfile =
  convertWith
    ConvertOptions
      { maybeInFormat = Nothing,
        maybeOutFormat = Nothing,
        inPath = infile,
        maybeOutPath = Just outfile,
        extraOptions = []
      }

convertTestWithFixUp :: (FilePath -> IO ()) -> FilePath -> FilePath -> IO ()
convertTestWithFixUp fixup infile outfile = do
  convertTest infile outfile
  fixup outfile

testGenerator :: InFormat -> TestTree
testGenerator inFormat =
  let basename = toLower <$> show inFormat
      inPath = "./examples/" <> basename <> toExtension inFormat
      testPaths =
        [ ( "./tests/output/" <> basename <> extension,
            "./examples/" <> basename <> extension
          )
          | extension <- allOutExtensions
        ]
      fixup fp = case fromExtension $ takeExtension fp of
        PDF -> do
          --  reset all date tags
          str <- BS.readFile fp
          print "date instances in pdf:"
          dates <- findAllBetween "(D:" ")" str
          print dates
          let anyDate :: ParsecT Void BS.ByteString IO BS.ByteString
              anyDate = choice (try <$> (chunk <$> dates))
          str <- streamEditT anyDate (\a -> return "20220830151034+02'00") str

          -- strictly renumber ID tags to counter race conditions observed in mermaid
          -- elements are correctly ordered but the IDs can differ from one run to the
          -- next
          print "renumber ids to follow strict order"
          ids <- findAllBetween "/ID (" ")" str
          print ids
          let anyId :: ParsecT Void BS.ByteString IO BS.ByteString
              anyId = choice (try <$> (chunk <$> ids))
              fixedNames :: M.Map BS.ByteString BS.ByteString
              fixedNames = M.fromList [
                  (nodeName,
                  let int = show nodePosition
                    in BSC.pack $ "node" <> replicate (8 - length int) '0' <> int
                  )
                | (nodePosition , nodeName) <- zip [1..] ids]
          str <- streamEditT anyId (\a -> return $ fromMaybe "notFound" (M.lookup a fixedNames)) str
          
          BS.writeFile fp str

        SVG -> do
          str <- BS.readFile fp
          print "id instances in svg:"
          ids <- findAllBetween "<svg id=\"" "\"" str
          print ids
          let anyId :: ParsecT Void BS.ByteString IO BS.ByteString
              anyId = choice (try <$> (chunk <$> ids))
          str <- streamEditT anyId (\a -> return "equalizedId") str
          BS.writeFile fp str
        _ -> return ()

   in testGroup (basename <> " tests") $
        [ goldenVsFile (basename <> " " <> show inFormat <> " test") goldenPath outPath $
            convertTestWithFixUp fixup inPath outPath
          | (outPath, goldenPath) <- testPaths
        ]

svgbobTest =
  let infile = "./examples/svgbob.bob"
      goldenfile = "./examples/svgbob.svg"
      outfile = "tests/output/svgbob.svg"
   in goldenVsFile
        "test svgbob example"
        goldenfile
        outfile
        (convertTest infile outfile)

plantumlTest =
  let infile = "./examples/plantuml.puml"
      goldenfile = "./examples/plantuml.svg"
      outfile = "tests/output/plantuml.svg"
   in goldenVsFile
        "test plantuml example"
        goldenfile
        outfile
        (convertTest infile outfile)

-- helpers
replaceFixup ::
  BS.ByteString ->
  BS.ByteString ->
  BS.ByteString ->
  BS.ByteString ->
  IO BS.ByteString
replaceFixup before after replacement str = do
  print "fixing up non-reproducible elements of file"
  let parser = betweenMatcher before after
  streamEditT parser (\a -> do
          print $ "parser found " <> show a <> ", replacing with " <> show replacement
          return $ before <> replacement <> after)
          str

findAllBetween :: BS.ByteString -> BS.ByteString -> BS.ByteString -> IO [BS.ByteString]
findAllBetween before after str = do
        let parser = rights <$> sepCap (betweenMatcher before after)
        parse <- runParserT parser "findAll" str
        case parse of
          Left err -> print err >> return []
          Right results -> return results

betweenMatcher :: BS.ByteString -> BS.ByteString -> ParsecT Void BS.ByteString IO BS.ByteString
betweenMatcher before after = do
  _ <- chunk before
  ret <- manyTill anySingle (chunk after)
  return $ BS.pack ret

betweenParser :: BS.ByteString -> BS.ByteString -> ParsecT Void BS.ByteString IO BS.ByteString
betweenParser before after = do
  _ <- manyTill anySingle (chunk before)
  ret <- manyTill anySingle (chunk after)
  return $ BS.pack ret
