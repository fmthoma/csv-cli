{-# LANGUAGE OverloadedStrings #-}

module Text.Csv.Parser (
    parseCsv
  , csv
) where

import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Text as T
import           Data.Text (Text)

import qualified Text.Csv.Types as Csv
import           Text.Csv.Types (Csv(..), Record, Header)



parseCsv :: Text -> Either String (Csv Text)
parseCsv = parseOnly csv

csv :: Parser (Csv Text)
csv = do
    hd <- record <* endOfLine
    recs <- record `sepBy'` endOfLine
    endOfInput
    return $ Csv (Csv.header hd, Csv.records recs)

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
