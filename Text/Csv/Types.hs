{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Csv.Types (
    Csv ()
  , fromList
  , getHeader
  , getRecords
  , toList

  , Header ()
  , header
  , getColumns

  , Record ()
  , record
  , records
  , getFields

  , filterCols
  , filterRows
) where


import           Data.Vector (Vector)
import qualified Data.Vector as V



newtype Csv a = Csv (Header a, [Record a]) deriving (Eq, Ord, Show)

fromList :: [[a]] -> Csv a
fromList (h:rs) = Csv (header h, records rs)

getHeader :: Csv a -> Header a
getHeader (Csv (h, _)) = h

getRecords :: Csv a -> [Record a]
getRecords (Csv (_, rs)) = rs

toList :: Csv a -> [[a]]
toList (Csv (h, rs)) = getColumns h : map getFields rs


newtype Header a = Header (Vector a) deriving (Eq, Ord, Show, Functor)

header :: [a] -> Header a
header = Header . V.fromList

getColumns :: Header a -> [a]
getColumns (Header h) = V.toList h


newtype Record a = Record (Vector a) deriving (Eq, Ord, Show, Functor)

record :: [a] -> Record a
record = Record . V.fromList

records :: [[a]] -> [Record a]
records = map record

getFields :: Record a -> [a]
getFields (Record r) = V.toList r



filterCols :: Eq a => [a] -> Csv a -> Maybe (Csv a)
filterCols cols (Csv (Header header, records)) = do
    colIndices <- sequence (fmap (\col -> V.findIndex (== col) header) cols)
    return $ Csv ( Header (keep colIndices header)
                 , map (\(Record rec) -> Record (keep colIndices rec)) records
                 )
  where
    keep colIndices = V.ifilter (\i _ -> i `elem` colIndices)

filterRows :: Eq a => a -> (a -> Bool) -> Csv a -> Csv a
filterRows col p csv@(Csv (Header header, _)) =
    let (Just colIndex) = V.findIndex (== col) header
        colPred (Record rec) = p (rec V.! colIndex)
    in  filterRows' colPred csv

filterRows' :: (Record a -> Bool) -> Csv a -> Csv a
filterRows' p (Csv (header, records)) = Csv ( header
                                            , filter p records
                                            )
