module Stagen.JsonFeed where

import qualified Data.ByteString.Lazy as BL
import Data.Aeson (Value(String))
import Data.Text (Text)
import Data.Text.Conversions (fromText, toText)
import Network.URI
import JsonFeed
import Data.Maybe

import Stagen.Page
import Stagen.Date

createJsonFeed :: String -> String -> [Page] -> BL.ByteString
createJsonFeed title baseUrl pages = let
  feed = mkFeed (toText title) (toText baseUrl)
  feed' = feed { feedItems = catMaybes $ map toItem pages }
  in renderFeed feed'

mkFeed :: Text -> Text -> Feed
mkFeed title baseUrl = Feed
  { feedAuthor = Nothing
  , feedDescription = Nothing
  , feedExpired = Nothing
  , feedFavicon = Nothing
  , feedFeedUrl = fmap (\uri -> Url $ uri { uriPath = (uriPath uri) ++ "/feed.json" }) baseUri
  , feedHomePageUrl = fmap Url baseUri
  , feedHubs = Nothing
  , feedIcon = Nothing
  , feedItems = []
  , feedNextUrl = Nothing
  , feedTitle = title
  , feedUserComment = Nothing
  , feedVersion = Url $ URI "https:" (Just $ URIAuth "" "jsonfeed.org" "") "/version/1" "" ""
  }
  where
    baseUri = parseURI (fromText baseUrl)

toItem :: Page -> Maybe Item
toItem page = case pageDate page of
  Nothing -> Nothing
  Just date -> Just Item
    { itemAttachments = Nothing
    , itemAuthor = Nothing
    , itemBannerImage = Nothing
    , itemContentHtml = Nothing
    , itemContentText = Nothing
    , itemDateModified = Nothing
    , itemDatePublished = Just $ dateToUTCTime date
    , itemExternalUrl = Nothing
    , itemId = String (toText (pageAbsoluteUrl page))
    , itemImage = Nothing
    , itemSummary = Nothing
    , itemTags = Nothing
    , itemTitle = Just (toText (pageTitle page))
    , itemUrl = fmap Url $ parseURI (fromText $ toText (pageAbsoluteUrl page))
    }
