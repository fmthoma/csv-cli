{-# LANGUAGE OverloadedStrings #-}

import           Data.Foldable
import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T

import Text.Csv
import Turtle
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
        (liftA Filter (argText "pattern" Default))
        mempty
    pretty = command "pretty" $ info
        (pure Pretty)
        mempty
    select = command "select" $ info
        (liftA Select (argText "columns" "A comma-separated list of columns"))
        mempty


main = do
    m <- options "Filters and pretty-prints CSV files" mode
    case m of
        Columns -> do
            Csv (Header h, _) <- fmap (either error id . parseCsv) T.getContents
            traverse_ T.putStrLn h

        Filter pattern ->
            undefined

        Pretty ->
            T.interact (T.unlines . align . either error id . parseCsv)

        Select cols -> do
            let selectedCols = fmap T.strip (T.splitOn "," cols)
            baseCsv <- fmap (either error id . parseCsv) T.getContents
            let (Just filteredCsv) = filterCols selectedCols baseCsv
            (T.putStr . T.unlines . printCsv) filteredCsv
