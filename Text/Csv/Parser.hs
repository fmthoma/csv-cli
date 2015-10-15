{-# LANGUAGE OverloadedStrings #-}

module Text.Csv.Parser (
    parseCsv
  , csv
) where

import           Control.Applicative
import           Data.Attoparsec.Text.Lazy
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)

import qualified Text.Csv.Types as Csv
import           Text.Csv.Types (Csv(..), Record, Header)



parseCsv :: [Text] -> Csv Text
parseCsv = Csv.fromList . fmap doParseRecord
  where
    doParseRecord line = case parse (record <* endOfInput) line of
        Fail _ _ msg -> [T.empty]
        Done _ csv   -> csv

csv :: Parser (Csv Text)
csv = do
    hd <- record <* endOfLine
    recs <- record `sepBy'` endOfLine
    return $ Csv.fromList (hd:recs)

record :: Parser [Text]
record = (quotedField <|> field) `sepBy` char ','

quotedField :: Parser Text
quotedField = T.pack <$> (between '"' textWithEscapedQuotes)
  where
    between c p = char c *> p <* char c
    textWithEscapedQuotes = many (notAQuote <|> escapedQuote)
    notAQuote = notChar '"'
    escapedQuote = string "\"\"" *> return '"'

field :: Parser Text
field = T.pack <$> many (satisfy (notInClass "\r\n\","))
