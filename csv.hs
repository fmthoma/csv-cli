{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Prelude hiding (FilePath)
import           Control.Applicative
import           Data.Foldable
import qualified Data.List as L
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import           Data.Optional
import           Filesystem.Path.CurrentOS
import qualified Options.Applicative as Opt
import           Turtle hiding (Text)

import Text.Csv


data Command
    = Columns
    | Filter { pattern :: Text }
    | Pretty { ppOptions :: PrettyPrintOptions }
    | Select { cols :: Text }

mode :: Parser (Command, Maybe FilePath)
mode = liftA2 (,) subcommand file
  where
    subcommand = Opt.subparser (columns <> filter <> pretty <> select)

    columns = command "columns" "Show columns of a csv file." $
        pure Columns

    filter = command "filter" "Filter rows of a csv file." $
        liftA (Filter . T.fromStrict)
            (argText "pattern" "<col>=<value>: Keep only rows where <col> is equal to <value>")

    pretty = command "pretty" "Pretty-print a csv file to a more human-readable table" $
        liftA Pretty $ liftA2 PPOpt
            (optional (optInt "max-width" 'w' "Break long lines after a number of characters"))
            (optional (optInt "repeat-header" 'r' "Repeat header after n lines"))

    select = command "select" "Show only selected columns." $
        liftA (Select . T.fromStrict)
            (argText "columns" "A comma-separated list of columns")

    command name description parser = Opt.command name $ Opt.info
        (Opt.helper <*> parser)
        (Opt.header description)

file :: Parser (Maybe FilePath)
file = optional (argPath "file" "Read csv data from a file. If no file is specified, read from stdin instead")


main = do
    (command, maybeFile) <- options "Filters and pretty-prints CSV files" mode
    input <- case maybeFile of
        Just file -> T.readFile (encodeString file)
        Nothing   -> T.getContents
    runCommand command input


runCommand :: Command -> Text -> IO ()
runCommand Columns
    = withCsv (getColumns . getHeader)

runCommand Filter {..}
    = withCsv $ \baseCsv ->
        let (column, value) = T.tail <$> T.breakOn "=" pattern
            filteredCsv = filterRows column (== value) baseCsv
        in printCsv filteredCsv

runCommand Pretty {..}
    = withCsv (prettyPrint ppOptions)

runCommand Select {..}
    = withCsv $ \baseCsv ->
        let selectedCols = fmap T.strip (T.splitOn "," cols)
            (Just filteredCsv) = filterCols selectedCols baseCsv
        in  printCsv filteredCsv


withCsv :: (Csv Text -> [Text]) -> Text -> IO ()
withCsv action = T.putStr . T.unlines . action . parseCsv . T.lines
