{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Prelude
import           Control.Applicative
import           Control.Monad
import           Data.Char (toUpper)
import qualified Data.List as L
import           Data.Monoid
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Options.Applicative as Opt
import           System.FilePath
import           System.Directory

import Text.Csv



data Command
    = Columns
    | Filter { invert :: Bool, pattern :: Text }
    | Pretty { ppOptions :: PrettyPrintOptions }
    | Select { invert :: Bool, cols :: Text }

mode :: Opt.Parser (Command, Maybe FilePath)
mode = Opt.subparser (columns <> filter <> pretty <> select)
  where
    columns = command "columns" "Show columns of a csv file." $
        pure Columns

    filter = command "filter" "Filter rows of a csv file." $
        liftA2 Filter
            (switch "invert" 'v' "Invert the selection (all columns except the given ones)." mempty)
            (argText "pattern" "<col>=<value>: Keep only rows where <col> is equal to <value>" mempty)

    pretty = command "pretty" "Pretty-print a csv file to a more human-readable table" $
        liftA Pretty $ liftA2 PPOpt
            (optional (opt "max-width" 'w' "Break long lines after a number of characters" mempty))
            (optional (opt "repeat-header" 'r' "Repeat header after n lines" mempty))

    select = command "select" "Show only selected columns." $
        liftA2 Select
            (switch "invert" 'v' "Invert the selection (all columns except the given ones)." mempty)
            (argText "columns" "A comma-separated list of columns" mempty)

    command name description parser = Opt.command name $ info description (liftA2 (,) parser file)

    file = optional (argPath "file" "Read csv data from a file. If no file is specified, read from stdin instead" mempty)



info :: String -> Opt.Parser a -> Opt.ParserInfo a
info description parser = Opt.info
    (Opt.helper <*> parser)
    (Opt.header description)

arg :: String -> String -> Opt.Mod Opt.ArgumentFields String -> Opt.Parser String
arg name description options = Opt.strArgument $ options
    <> (Opt.metavar . map toUpper) name
    <> Opt.help description

argText :: String -> String -> Opt.Mod Opt.ArgumentFields String -> Opt.Parser Text
argText name description options = fmap T.pack (arg name description options)

argPath :: String -> String -> Opt.Mod Opt.ArgumentFields String -> Opt.Parser FilePath
argPath name description options = arg name description $ options
    <> Opt.completer (Opt.mkCompleter csvFileCompletion)
  where
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

opt :: Read a => String -> Char -> String -> Opt.Mod Opt.OptionFields a -> Opt.Parser a
opt long short description options = Opt.option Opt.auto $ options
    <> (Opt.metavar . map toUpper) long
    <> Opt.long long
    <> Opt.short short
    <> Opt.help description

switch :: String -> Char -> String -> Opt.Mod Opt.FlagFields Bool -> Opt.Parser Bool
switch long short description options = Opt.switch $ options
    <> Opt.long long
    <> Opt.short short
    <> Opt.help description




main = do
    (command, maybeFile) <- Opt.execParser (info "Filters and pretty-prints CSV files" mode)
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
            predicate = if invert
                then (/= value)
                else (== value)
            filteredCsv = filterRows column predicate baseCsv
        in printCsv filteredCsv

runCommand Pretty {..}
    = withCsv (prettyPrint ppOptions)

runCommand Select {..}
    = withCsv $ \baseCsv ->
        let specifiedCols = fmap T.strip (T.splitOn "," cols)
            selectedCols = if invert
                then (getColumns . getHeader) baseCsv L.\\ specifiedCols
                else specifiedCols
            (Just filteredCsv) = filterCols selectedCols baseCsv
        in  printCsv filteredCsv


withCsv :: (Csv Text -> [Text]) -> Text -> IO ()
withCsv action = T.putStr . T.unlines . action . parseCsv . T.lines
