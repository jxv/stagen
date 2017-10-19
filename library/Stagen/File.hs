module Stagen.File where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Control.Monad (when)
import System.FilePath.Find
import Text.Markdown
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Stagen.Opts
import Stagen.Page

fromMarkdown :: Verbose -> FilePath -> IO (FilePath, Page)
fromMarkdown verbose mdPath = do
    let htmlPath = changeExtension mdPath "html"
    page <- readPage mdPath
    when (verbose == Verbose) (putStrLn htmlPath)
    return (htmlPath, page)

eligable :: [FilePath] -> FindClause Bool
eligable ignore = do
    isMarkdown <- (== ".md") <$> extension
    name <- fileName
    let isFileName = and (map (/= name) ignore)
    return (isMarkdown && isFileName)

readPage :: FilePath -> IO Page
readPage path = do
    content <- TL.readFile path
    let pageTitle = TL.filter (not . isMarkdownChar) (TL.takeWhile isNotNewLine content)
    let pageContent = render content
    return Page{..}
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
