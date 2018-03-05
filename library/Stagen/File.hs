module Stagen.File where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import Data.Text.Conversions (toText)
import Data.Monoid ((<>))
import Control.Monad (when)
import System.FilePath.Find
import Text.Markdown
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Stagen.Opts
import Stagen.Page
import Stagen.Date

fromMarkdown :: Verbose -> FilePath -> FilePath -> IO (FilePath, Page)
fromMarkdown verbose baseUrl mdPath = do
    let htmlPath = changeExtension mdPath "html"
    page <- readPage baseUrl mdPath
    when (verbose == Verbose) (putStrLn htmlPath)
    return (htmlPath, page)

eligable :: [FilePath] -> FindClause Bool
eligable ignore = do
    isMarkdown <- (== ".md") <$> extension
    name <- fileName
    let isFileName = and (map (/= name) ignore)
    return (isMarkdown && isFileName)

absoluteUrl :: FilePath -> FilePath -> TL.Text
absoluteUrl baseUrl path = TL.fromStrict . toText  $ changeExtension (baseUrl <> "/" <> path) "html"

readPage :: FilePath -> FilePath -> IO Page
readPage baseUrl path = do
    content <- TL.readFile path
    let pageAbsoluteUrl = absoluteUrl baseUrl (drop 2 path)
    let pageDate = parseMay datePrefix (baseName path)
    let pageTitle = mkTitle content
    let pageContent = render content
    return Page{..}

mkTitle :: TL.Text -> TL.Text
mkTitle content = TL.filter (not . isMarkdownChar) (TL.takeWhile isNotNewLine content)
  where
    isMarkdownChar ch = ch == '_' || ch == '*' || ch == '#' || ch == '>'
    isNotNewLine ch = ch /= '\n' && ch /= '\r'

changeExtension :: FilePath -> String -> FilePath
changeExtension path newExtension
    | hasExt = basename ++ newExtension
    | otherwise = path ++ ('.' : newExtension)
 where
    revPath = reverse path
    hasExt = elem '.' (takeWhile (/= '/') revPath)
    basename = reverse (dropWhile (/= '.') revPath)

render :: TL.Text -> TL.Text
render = renderHtml . markdown def

parseMay :: P.Parser a -> String -> Maybe a
parseMay p src = case P.parse p "" src of
  Left _ -> Nothing
  Right x -> Just x

baseName :: FilePath -> FilePath
baseName path = basename
 where
    revPath = reverse path
    revName = takeWhile (/= '/') revPath
    basename = reverse (dropWhile (/= '.') revName)
