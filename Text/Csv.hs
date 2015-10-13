{-# LANGUAGE OverloadedStrings #-}

module Text.Csv (
    module Text.Csv.Parser
  , module Text.Csv.Pretty
  , module Text.Csv.Types

) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Text.Csv.Parser
import Text.Csv.Pretty
import Text.Csv.Types



printCsv :: Csv Text -> [Text]
printCsv (Csv (hd, recs)) = printHeader hd : printRecords recs
  where
    printHeader (Header header) = (join . fmap printField) header
    printRecords = fmap printRecord
    printRecord (Record record) = (join . fmap printField) record
    join = T.intercalate "," . V.toList
    printField field = T.concat ["\"", escape field, "\""]
    escape = T.replace "\"" "\"\""
