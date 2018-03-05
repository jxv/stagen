module Stagen.Feed where

import qualified Data.Text.Lazy as TL
import qualified Text.Atom.Feed as Atom
-- import qualified Text.Feed.Types as Feed
-- import qualified Text.RSS.Syntax as RSS
import qualified Text.Atom.Feed.Export as Export

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

createAtomFeed :: String -> FilePath -> [Page] -> Maybe TL.Text
createAtomFeed title baseUrl
  = fmap TL.fromStrict
  . feed title baseUrl
  . catMaybes
  . map (\page -> case pageDate page of
      Nothing -> Nothing
      Just date -> Just (toAtomDate date, toStrict $ pageAbsoluteUrl page, Atom.TextString . toStrict $ pageTitle page))

toAtomDate :: Date -> Atom.Date
toAtomDate Date{dateYear,dateMonth,dateDay} = toText $ mconcat
  [ show dateYear
  , "-"
  , showTwoDigits dateMonth
  , "-"
  , showTwoDigits dateDay
  , "T00:00:00Z"
  ]

feed :: String -> FilePath -> [(Atom.Date, Atom.URI, Atom.TextContent)] -> Maybe Text
feed blogTitle baseUrl posts =
  fmap (toStrict . renderText def) $
  elementToDoc $
  Export.xmlFeed $
  fd {Atom.feedEntries = fmap toEntry posts, Atom.feedLinks = [Atom.nullLink $ toText baseUrl]}
  where
    fd :: Atom.Feed
    fd =
      Atom.nullFeed
        (toText baseUrl `mappend` "/atom.xml") -- ID
        (Atom.TextString $ toText blogTitle) -- Title
        (case posts -- Updated
               of
           (latestPostDate, _, _):_ -> latestPostDate
           _ -> "")
    toEntry :: (Atom.Date, Atom.URI, Atom.TextContent) -> Atom.Entry
    toEntry (date, url, title) =
      (Atom.nullEntry
         url -- The ID field. Must be a link to validate.
         title -- Title
         date)
      { Atom.entryAuthors = [Atom.nullPerson]
      , Atom.entryLinks = [Atom.nullLink url]
      , Atom.entryContent = Nothing
      }

elementToDoc :: XML.Element -> Maybe C.Document
elementToDoc el =
  either (const Nothing) Just $ fromXMLDocument $ XML.Document (Prologue [] Nothing []) el []
