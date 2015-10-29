{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Prelude
import           Control.Applicative
import           Control.Monad
import           Data.Char (toUpper)
import           Data.Foldable
import qualified Data.List as L
import           Data.Monoid
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import           Data.Optional
import qualified Options.Applicative as Opt
import           System.FilePath
import           System.Directory
import           Text.Read (readMaybe)

import Text.Csv

import Turtle (options)

data Command
    = Columns
    | Filter { pattern :: Text }
    | Pretty { ppOptions :: PrettyPrintOptions }
    | Select { cols :: Text }

mode :: Opt.Parser (Command, Maybe FilePath)
mode = Opt.subparser (columns <> filter <> pretty <> select)
  where
    columns = command "columns" "Show columns of a csv file." $
        pure Columns

    filter = command "filter" "Filter rows of a csv file." $
        liftA Filter
            (argText "pattern" "<col>=<value>: Keep only rows where <col> is equal to <value>" mempty)

    pretty = command "pretty" "Pretty-print a csv file to a more human-readable table" $
        liftA Pretty $ liftA2 PPOpt
            (optional (opt "max-width" 'w' "Break long lines after a number of characters" mempty))
            (optional (opt "repeat-header" 'r' "Repeat header after n lines" mempty))

    select = command "select" "Show only selected columns." $
        liftA Select
            (argText "columns" "A comma-separated list of columns" mempty)

    command name description parser = Opt.command name $ Opt.info
        (Opt.helper <*> (liftA2 (,) parser file))
        (Opt.header description)

    file = optional (argPath "file" "Read csv data from a file. If no file is specified, read from stdin instead" mempty)

    arg name description options = Opt.strArgument $ options
        <> (Opt.metavar . map toUpper) name
        <> Opt.help description

    argText name description options = fmap T.pack (arg name description options)

    argPath name description options = fmap (T.unpack) . argText name description $ options
        <> Opt.completer (Opt.mkCompleter csvFileCompletion)

    opt long short description options = Opt.option Opt.auto $ options
        <> (Opt.metavar . map toUpper) long
        <> Opt.long long
        <> Opt.short short
        <> Opt.help description

csvFileCompletion :: FilePath -> IO [FilePath]
csvFileCompletion prefix = do
    let (dir, filePrefix) = splitFileName prefix
        showMetaFiles = filePrefix `elem` [".", ".."]
        isFile file = doesFileExist (dir </> file)
        isDir  file = doesDirectoryExist (dir </> file)
        isCsv  file = takeExtension file == ".csv"
        isMetaFile file = file `elem` [".", ".."]

    directoryContents <- getDirectoryContents dir
    csvFiles <- (filterM isFile . L.filter isCsv) directoryContents
    subdirs  <- filterM isDir directoryContents
    let candidates = if showMetaFiles
                        then csvFiles ++ subdirs
                        else L.filter (not . isMetaFile) (csvFiles ++ subdirs)

    let matchingFiles = L.filter (filePrefix `L.isPrefixOf`) candidates

    return (fmap (replaceFileName prefix) matchingFiles)



main = do
    (command, maybeFile) <- options "Filters and pretty-prints CSV files" mode
    input <- case maybeFile of
        Just file -> T.readFile file
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
