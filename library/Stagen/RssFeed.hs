module Stagen.RssFeed where

import qualified Data.Text.Lazy as TL
import qualified Text.RSS.Syntax as Rss
import qualified Text.RSS.Export as Export

import Prelude ()
import Prelude.Compat hiding (take)
import Data.Text hiding (map)
import Data.Text.Conversions (toText)
import Data.Text.Lazy (toStrict)
import Data.XML.Types as XML
import Text.XML as C
import Data.Maybe

import Stagen.Date
import Stagen.Page

createRssFeed :: String -> FilePath -> [Page] -> Maybe TL.Text
createRssFeed title baseUrl
  = fmap TL.fromStrict
  . feed title baseUrl
  . catMaybes
  . map (\page -> case pageDate page of
      Nothing -> Nothing
      Just date -> Just (toDateString date, toStrict $ pageAbsoluteUrl page, toStrict $ pageTitle page))

toDateString :: Date -> Rss.DateString
toDateString Date{dateYear,dateMonth,dateDay} = toText $ mconcat
  [ show dateYear
  , "-"
  , showTwoDigits dateMonth
  , "-"
  , showTwoDigits dateDay
  , "T00:00:00Z"
  ]

feed :: String -> FilePath -> [(Rss.DateString, Rss.URLString, Text)] -> Maybe Text
feed blogTitle baseUrl posts = fmap (toStrict . renderText def) $ elementToDoc $ Export.xmlRSS $ fd
  where
    fd :: Rss.RSS
    fd =
      (Rss.nullRSS
        (toText blogTitle) -- Title
        (toText $ baseUrl ++ "/rss.xml")) -- ID
        { Rss.rssChannel = (Rss.nullChannel (toText blogTitle) (toText baseUrl))
            { Rss.rssItems = map
                (\(date, url, title) -> (Rss.nullItem title) { Rss.rssItemLink = Just url, Rss.rssItemPubDate = Just date })
                posts
            }
        }

elementToDoc :: XML.Element -> Maybe C.Document
elementToDoc el =
  either (const Nothing) Just $ fromXMLDocument $ XML.Document (Prologue [] Nothing []) el []
