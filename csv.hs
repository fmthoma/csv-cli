{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Foldable
import qualified Data.List as L
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

import Prelude hiding (FilePath)
import Text.Csv
import Turtle hiding (Text)
import Data.Optional
import Filesystem.Path.CurrentOS

import qualified Options.Applicative as Opt

data Mode
    = Columns
    | Filter { pattern :: Text }
    | Pretty { maxWidth :: Maybe Int }
    | Select { cols :: Text }

mode :: Parser (Mode, Maybe FilePath)
mode =  liftA2 (,) subcommand file
  where
    subcommand = Opt.subparser (columns <> filter <> pretty <> select)

    columns = command "columns" "Show columns of a csv file." $
        pure Columns

    filter = command "filter" "Filter rows of a csv file." $
        liftA (Filter . T.fromStrict)
            (argText "pattern" "<col>=<value>\nKeep only rows where <col> is equal to <value>")

    pretty = command "pretty" "Pretty-print a csv file to a more human-readable table" $
        liftA Pretty
            (optional (optInt "max-width" 'w' "Break long lines after a number of characters"))

    select = command "select" "Show only selected columns." $
        liftA (Select . T.fromStrict)
            (argText "columns" "A comma-separated list of columns")

    command name description parser = Opt.command name $ Opt.info
        (Opt.helper <*> parser)
        (Opt.header description)

file :: Parser (Maybe FilePath)
file = optional (argPath "file" "Read csv data from a file. If no file is specified, read from stdin instead")

main = do
    (m, f) <- options "Filters and pretty-prints CSV files" mode
    input <- case f of
        Just file -> T.readFile (encodeString file)
        Nothing   -> T.getContents
    case m of
        Columns -> withCsv input (getColumns . getHeader)

        Filter pattern -> withCsv input $ \baseCsv ->
            let (column, value) = T.tail <$> T.breakOn "=" pattern
                filteredCsv = filterRows column (== value) baseCsv
            in printCsv filteredCsv

        Pretty maxWidth -> case maxWidth of
            Just w  -> withCsv input (alignMaxWidth w)
            Nothing -> withCsv input align

        Select cols -> withCsv input $ \baseCsv ->
            let selectedCols = fmap T.strip (T.splitOn "," cols)
                (Just filteredCsv) = filterCols selectedCols baseCsv
            in  printCsv filteredCsv


withCsv :: Text -> (Csv Text -> [Text]) -> IO ()
withCsv input action = (T.putStr . T.unlines . action . parseCsv . T.lines) input
