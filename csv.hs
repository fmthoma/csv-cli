{-# LANGUAGE OverloadedStrings #-}

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

import Options.Applicative as Opt

data Mode
    = Columns
    | Filter { pattern :: Text }
    | Pretty
    | Select { cols :: Text }

mode :: Parser (Mode, Maybe FilePath)
mode =  liftA2 (,) subcommand file
  where
    subcommand = subparser (columns <> filter <> pretty <> select)
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

file :: Parser (Maybe FilePath)
file = optional (argPath "file" Default)

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

        Pretty -> withCsv input align

        Select cols -> withCsv input $ \baseCsv ->
            let selectedCols = fmap T.strip (T.splitOn "," cols)
                (Just filteredCsv) = filterCols selectedCols baseCsv
            in  printCsv filteredCsv


withCsv :: Text -> (Csv Text -> [Text]) -> IO ()
withCsv input action = (T.putStr . T.unlines . action . parseCsv . T.lines) input
