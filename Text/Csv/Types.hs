{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Csv.Types (
    Csv (..)
  , Header (..)
  , header
  , Record (..)
  , record
  , records

  , filterCols
  , filterRows
  , toList
) where


import           Data.Vector (Vector)
import qualified Data.Vector as V



newtype Csv a = Csv (Header a, [Record a]) deriving (Eq, Ord, Show)


newtype Header a = Header (Vector a) deriving (Eq, Ord, Show, Functor)

header :: [a] -> Header a
header = Header . V.fromList


newtype Record a = Record (Vector a) deriving (Eq, Ord, Show, Functor)

record :: [a] -> Record a
record = Record . V.fromList

records :: [[a]] -> [Record a]
records = map record



filterCols :: Eq a => [a] -> Csv a -> Maybe (Csv a)
filterCols cols (Csv (Header header, records)) = do
    colIndices <- sequence (fmap (\col -> V.findIndex (== col) header) cols)
    return $ Csv ( Header (keep colIndices header)
                 , map (\(Record rec) -> Record (keep colIndices rec)) records
                 )
  where
    keep colIndices = V.ifilter (\i _ -> i `elem` colIndices)

filterRows :: (Record a -> Bool) -> Csv a -> Csv a
filterRows p (Csv (header, records)) = Csv ( header
                                           , filter p records
                                           )

toList :: Csv a -> [[a]]
toList (Csv (Header header, records)) =
    V.toList header : map (\(Record rec) -> V.toList rec) records
