module Main where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.List as L
import Options.Applicative
import Options.Applicative.Builder.Internal (HasName)
import System.FilePath.Find
import System.FilePath.Glob
import System.FilePath.Manip
import Text.Markdown
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Default
import Data.Monoid
import Data.Maybe

import Stagen.Opts (Opts(..), Command(..), optsP)
import Stagen.Page
import Stagen.Template

main :: IO ()
main = do
    opts@Opts{..} <- execParser (info optsP idm)
    case optsCommand of
        Init -> return ()
        Build -> runBuild opts
        Clean -> return ()

runBuild :: Opts -> IO ()
runBuild opts@Opts{..} = do
    tpl <- mkTemplate opts
    let ignore = catMaybes [optsHeader, optsFooter, optsArchive]
    files <- find (pure True) (eligable ignore) (optsTargetDirectory)
    mapM_ (writePageFromMarkdown tpl) files

changeExtension :: FilePath -> String -> FilePath
changeExtension path newExtension
    | hasExt = basename ++ newExtension
    | otherwise = path ++ ('.' : newExtension)
 where
    revPath = reverse path
    hasExt = elem '.' (takeWhile (/= '/') revPath)
    basename = reverse (dropWhile (/= '.') revPath)

writePageFromMarkdown :: Template -> FilePath -> IO ()
writePageFromMarkdown tpl mdPath = do
    page <- mkPage mdPath
    writePage tpl page (changeExtension mdPath "html")

writePage :: Template -> Page -> FilePath -> IO ()
writePage tpl page htmlPath = TL.writeFile htmlPath (construct tpl page)

eligable :: [FilePath] -> FindClause Bool
eligable ignore = do
    isMarkdown <- (== ".md") <$> extension
    name <- fileName
    let isFileName = and (map (/= name) ignore)
    return (isMarkdown && isFileName) 

mkTemplate :: Opts -> IO Template
mkTemplate Opts{..} = do
    let tplStyleSheets = map TL.pack optsStyleSheets
    let tplScripts = map TL.pack optsScripts
    tplHeader <- go optsHeader
    tplFooter <- go optsFooter
    return Template{..}
 where
    go Nothing = return Nothing
    go (Just path) = Just <$> render <$> TL.readFile path

mkPage :: FilePath -> IO Page
mkPage filePath = do
    let pageTitle = Nothing
    pageContent <- render <$> TL.readFile filePath
    return Page{..}

render :: TL.Text -> TL.Text
render = renderHtml . markdown def

construct :: Template -> Page -> TL.Text
construct Template{..} Page{..} = (html . TL.concat)
    [ (head' . TL.concat) (try pageTitle : map styleSheet tplStyleSheets ++ map script tplScripts)
    , (body . wrapper . TL.concat)
        [ divHeader (try tplHeader)
        , divContent pageContent
        , divFooter (try tplFooter) ] ]
 where
    try = fromMaybe TL.empty

html, head', title, styleSheet, script, body, wrapper, divHeader, divContent, divFooter :: TL.Text -> TL.Text
html x = "<!doctype html><html>" <> x <> "</html>"
head' x = "<head>" <> x <> "</head>"
title x = "<title>" <> x <> "</title>"
styleSheet x = "<link rel=\"stylesheet\" type=\"text/css\" href=\"" <> x <> "\">"
script x = "<script src=\"" <> x <> "\"></script>"
body x = "<body>" <> x <> "</body>"
wrapper x = "<div id=\"wrapper\">" <> x <> "</div>"
divHeader x = "<div id=\"header\">" <> x <> "</div>"
divContent x = "<div id=\"content\">" <> x <> "</div>"
divFooter x = "<div id=\"footer\">" <> x <> "</div>"
