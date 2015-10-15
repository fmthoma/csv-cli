{-# LANGUAGE OverloadedStrings #-}

import           Data.Foldable
import qualified Data.List as L
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

import Text.Csv
import Turtle hiding (Text)
import Data.Optional

import Options.Applicative as Opt

data Mode
    = Columns
    | Filter { pattern :: Text }
    | Pretty
    | Select { cols :: Text }

mode :: Parser Mode
mode = subparser (columns <> filter <> pretty <> select)
  where
    columns = command "columns" $ info
        (pure Columns)
        mempty
    filter = command "filter" $ info
        (liftA (Filter . T.fromStrict) (argText "pattern" Default))
        mempty
    pretty = command "pretty" $ info
        (pure Pretty)
        mempty
    select = command "select" $ info
        (liftA (Select . T.fromStrict) (argText "columns" "A comma-separated list of columns"))
        mempty


main = do
    m <- options "Filters and pretty-prints CSV files" mode
    case m of
        Columns -> withCsv (getColumns . getHeader)

        Filter pattern -> undefined

        Pretty -> withCsv align

        Select cols -> withCsv $ \baseCsv ->
            let selectedCols = fmap T.strip (T.splitOn "," cols)
                (Just filteredCsv) = filterCols selectedCols baseCsv
            in  printCsv filteredCsv


withCsv :: (Csv Text -> [Text]) -> IO ()
withCsv action = T.interact (T.unlines . action . parseCsv . T.lines)
