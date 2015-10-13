{-# LANGUAGE OverloadedStrings #-}

import           Data.Foldable
import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T

import Text.Csv


main = T.interact (T.unlines . printCsv . either error id . parseCsv)

