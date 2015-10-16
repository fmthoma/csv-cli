{-# LANGUAGE OverloadedStrings #-}

module Text.Csv.Pretty (
    align
  , alignMaxWidth
) where

import qualified Data.List as L
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import           Data.Monoid

import Text.Csv.Types



align :: Csv Text -> [Text]
align = map (T.intercalate " ") . alignCols . toList

alignMaxWidth :: Int -> Csv Text -> [Text]
alignMaxWidth w = map (T.intercalate " ") . alignCols . breakLongLines . toList
  where
    breakLongLines :: [[Text]] -> [[Text]]
    breakLongLines [] = []
    breakLongLines (l:ls) = (transpose . fmap (breakLong w)) l ++ breakLongLines ls



alignCols :: [[Text]] -> [[Text]]
alignCols = transpose . map alignCol . transpose

transpose :: Monoid a => [[a]] -> [[a]]
transpose = L.transpose . padTable

padTable :: Monoid a => [[a]] -> [[a]]
padTable rows = fmap (pad numCols) rows
  where
    numCols = maximum (map length rows)
    pad l cols = cols ++ replicate (l - length cols) mempty

alignCol :: [Text] -> [Text]
alignCol cells = fmap (pad maxLength) cells
  where
    maxLength = maximum (map T.length cells)
    pad l s = s `T.append` T.replicate (l - T.length s) " "

breakLong :: Int -> Text -> [Text]
breakLong lim s = if T.length s >= fromIntegral lim
    then let (s', rest) = breakWords s
         in  s' : breakLong lim rest
    else [s]
  where
    wordBoundaries = ".,;:| " :: String

    breakWords :: Text -> (Text, Text)
    breakWords s = let (l, r) = forceBreak s
                       (l1, r1) = spanEnd (not . (`elem` wordBoundaries)) l
                   in  if T.null l1 then (l, r) else (l1, r1 <> r)

    spanEnd :: (Char -> Bool) -> Text -> (Text, Text)
    spanEnd p s = let (r, l) = T.span p (T.reverse s)
                  in  (T.reverse l, T.reverse r)

    forceBreak :: Text -> (Text, Text)
    forceBreak = T.splitAt (fromIntegral lim)
