{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Csv.Pretty (
    PrettyPrintOptions(..)
  , prettyPrint
) where

import qualified Data.List as L
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Monoid

import Text.Csv.Types



data PrettyPrintOptions
    = PPOpt { maxWidth :: Maybe Int
            , repeatHeader :: Maybe Int
            }

prettyPrint :: PrettyPrintOptions -> Csv Text -> [Text]
prettyPrint PPOpt{..} csv =
    let aligned = align maxWidth csv
        hd = head aligned
        bd = tail aligned
    in  formatRepeatedHeader repeatHeader hd bd




formatRepeatedHeader :: Maybe Int -> Text -> [Text] -> [Text]
formatRepeatedHeader maybeRepeat hd bd = case maybeRepeat of
    Nothing -> hd : sep : bd
    Just n  -> sep : hd : sep : take n bd
            ++ formatRepeatedHeader (Just n) hd (drop n bd)
  where
    sep = T.replicate (T.length hd) "═"

align :: Maybe Int -> Csv Text -> [Text]
align Nothing
    = map (T.intercalate " ") . alignCols . toList

align (Just w)
    = \csv ->
        let breakHeader = fmap (T.take (fromIntegral w)) . getColumns . getHeader
            breakBody = breakLongLines w . fmap getFields . getRecords
        in  (map (T.intercalate " ") . alignCols) (breakHeader csv : breakBody csv)


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

breakLongLines :: Int -> [[Text]] -> [[Text]]
breakLongLines _ [] = []
breakLongLines w (l:ls) = (transpose . fmap (breakLongLine w)) l ++ breakLongLines w ls

breakLongLine :: Int -> Text -> [Text]
breakLongLine lim s = if T.length s >= fromIntegral lim
    then let (s', rest) = breakWords s
         in  s' : breakLongLine lim rest
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
