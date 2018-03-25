{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import           Control.DeepSeq
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.CSV.Conduit
import qualified Data.Csv
import qualified Data.Sv.Parse
import qualified Data.Text.Encoding as T
import           Data.Vector (Vector)
import           System.Directory
import qualified Text.CSV
import qualified Text.Parsec.Error
import           Weigh

main :: IO ()
main = do
  let fp = "out.csv"
  exists <- doesFileExist fp
  when exists (removeFile fp)
  mainWith
    (do setColumns [Case, Allocated, Max, Live, GCs]
        sequence_
          [ action
              "cassava/decode/Vector ByteString"
              (do r <-
                    fmap (Data.Csv.decode Data.Csv.HasHeader) (L.readFile infp) :: IO (Either String (Vector (Vector ByteString)))
                  case r of
                    Left _ -> error "Unexpected parse error"
                    Right v -> pure v)
          , action
              "cassava/decode/[ByteString]"
              (do r <-
                    fmap (Data.Csv.decode Data.Csv.HasHeader) (L.readFile infp) :: IO (Either String (Vector [ByteString]))
                  case r of
                    Left _ -> error "Unexpected parse error"
                    Right v -> pure v)
          , action
              "csv-conduit/readCSVFile/[ByteString]"
              (Data.CSV.Conduit.readCSVFile Data.CSV.Conduit.defCSVSettings infp :: IO (Vector [ByteString]))
          , action
              "csv-conduit/readCSVFile/Vector ByteString"
              (Data.CSV.Conduit.readCSVFile Data.CSV.Conduit.defCSVSettings infp :: IO (Vector (Vector ByteString)))
          , action
              "csv-conduit/readCSVFile/[String]"
              (Data.CSV.Conduit.readCSVFile Data.CSV.Conduit.defCSVSettings infp :: IO (Vector [String]))
          , action
              "csv/Text.CSV/parseCSVFromFile"
              (Text.CSV.parseCSVFromFile infp)
          , action
              "sv/Data.Sv.Parse/attoparsecText"
              (Data.Sv.Parse.parseSvFromFile'
                 Data.Sv.Parse.attoparsecText
                 (fmap T.decodeUtf8 Data.Sv.Parse.defaultParseOptions)
                 infp)
          , action
              "sv/Data.Sv.Parse/attoparsecByteString"
              (Data.Sv.Parse.parseSvFromFile'
                 Data.Sv.Parse.attoparsecByteString
                 Data.Sv.Parse.defaultParseOptions
                 infp)
          , action
              "sv/Data.Sv.Parse/trifecta"
              (Data.Sv.Parse.parseSvFromFile'
                 Data.Sv.Parse.trifecta
                 Data.Sv.Parse.defaultParseOptions
                 infp)
          ])

-- | We don't need to force error messages, the test suite only parses
-- valid CSV files.
instance NFData Text.Parsec.Error.ParseError where
  rnf _ = error "Unexpected parse error."

infp :: FilePath
infp = "in.csv"
