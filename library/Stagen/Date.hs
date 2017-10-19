module Stagen.Date where

import Data.Functor (void)
import Text.Parsec
import Text.Parsec.String

data Date = Date {
    dateYear :: Int,
    dateMonth :: Int,
    dateDay :: Int
} deriving (Show, Eq, Ord)

datePrefix :: Parser Date
datePrefix = do
    year <- number
    void $ char '-'
    month <- number
    void $ char '-'
    day <- number
    void $ char '-'
    return (Date year month day)
 where
    number = fmap read (many1 digit)
