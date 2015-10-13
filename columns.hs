{-# LANGUAGE OverloadedStrings #-}

import           Data.Foldable
import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T


separator :: Text
separator = ","


main = T.interact prettyPrintColumns


prettyPrintColumns :: Text -> Text
prettyPrintColumns = T.unlines . alignAtSeparator . T.lines

alignAtSeparator :: [Text] -> [Text]
alignAtSeparator = map (T.intercalate " ") . alignCols . map parseLine

parseLine :: Text -> [Text]
parseLine = T.splitOn separator


alignCols :: [[Text]] -> [[Text]]
alignCols = transpose . map alignCol . transpose

transpose :: Monoid a => [[a]] -> [[a]]
transpose rows = (L.transpose . fmap (pad numCols)) rows
  where
    numCols = maximum (map length rows)
    pad l cols = cols ++ replicate (l - length cols) mempty

alignCol :: [Text] -> [Text]
alignCol cells = fmap (pad maxLength) cells
  where
    maxLength = maximum (map T.length cells)
    pad l s = s `T.append` T.replicate (l - T.length s) " "
