{-# LANGUAGE OverloadedStrings #-}

module Text.Csv (
    module Text.Csv.Pretty
  , module Text.Csv.Types

  , parseCsv
  , printCsv
) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Text.Csv.Parser
import Text.Csv.Pretty
import Text.Csv.Types



printCsv :: Csv Text -> [Text]
printCsv = fmap printRecord . toList
  where
    printRecord = join . fmap printField
    join = T.intercalate ","
    printField field = T.concat ["\"", escape field, "\""]
    escape = T.replace "\"" "\"\""
