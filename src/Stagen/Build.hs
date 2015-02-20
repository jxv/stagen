module Stagen.Build where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.List as L
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import Control.Arrow
import Control.Monad (when, void)
import Options.Applicative
import Options.Applicative.Builder.Internal (HasName)
import System.FilePath.Find
import System.FilePath.Glob
import System.FilePath.Manip
import System.IO.Error (catchIOError)
import Text.Blaze.Html (Html)
import Data.Default
import Data.Monoid
import Data.Maybe

import Stagen.Opts
import Stagen.Date
import Stagen.Template
import Stagen.Page
import Stagen.Job
import Stagen.File

runBuild :: Opts -> IO ()
runBuild opts@Opts{..} = do
    tpl <- mkTemplate opts
    let ignore = optsIgnore ++ catMaybes [optsHeader, optsFooter, optsArchive]
    files <- find (pure True) (eligable ignore) optsTargetDirectory
    htmlPathAndPages <- sequence $ map (fromMarkdown optsVerbose) files
    let archive = archiveJob optsVerbose tpl htmlPathAndPages optsArchive
    let pageAndHtmlPaths = map (\(page, path) -> (path, page)) htmlPathAndPages
    let jobs = archive : map (uncurry (writePage tpl)) pageAndHtmlPaths
    void $ runJobs optsJobs jobs

build :: Template -> Page -> TL.Text
build Template{..} Page{..} = (html . TL.concat)
    [ (head' . TL.concat) (title pageTitle : map styleSheet tplStyleSheets ++ map script tplScripts)
    , (body . wrapper . TL.concat)
        [ divHeader (try tplHeader)
        , divContent pageContent
        , divFooter (try tplFooter) ] ]
 where
    try = fromMaybe TL.empty

archiveJob :: Verbose -> Template -> [(FilePath, Page)] -> Maybe FilePath -> IO ()
archiveJob verbose tpl htmlPathAndPages mMdPath = fromMaybe (return ()) $ do
    mdPath <- mMdPath
    return $ do
        (htmlPath, page) <- fromMarkdown verbose mdPath
        writePage tpl (addArchiveEntries page htmlPathAndPages) htmlPath

writePageFromMarkdown :: Verbose -> Template -> FilePath -> IO ()
writePageFromMarkdown verbose tpl mdPath = do
    (htmlPath, page) <- fromMarkdown verbose mdPath
    writePage tpl page htmlPath

writePage :: Template -> Page -> FilePath -> IO ()
writePage tpl page htmlPath = TL.writeFile htmlPath (build tpl page)

addArchiveEntries :: Page -> [(FilePath, Page)] -> Page
addArchiveEntries page htmlPathAndPages =
    let pathAndTitles = map (second pageTitle) htmlPathAndPages
        sorter = L.sortBy (\(_,_,a) (_,_,b)-> compare a b)
        content = (pageContent page) <> (toLinks . sorter . getEntries) pathAndTitles
    in Page{pageTitle = pageTitle page, pageContent = content}
 where
    getEntries :: [(FilePath, TL.Text)] -> [(FilePath, TL.Text, Date)]
    getEntries = catMaybes . map (\(f,t) -> fmap (f,t,) (parseMay datePrefix (baseName f)))

    toLinks :: [(FilePath, TL.Text, Date)] -> TL.Text
    toLinks = ul . TL.concat . map toLink

    toLink :: (FilePath, TL.Text, Date) -> TL.Text
    toLink (path, title, date) = li (anchor (TL.pack path) title)

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

html, head', title, styleSheet, script, body, wrapper, ul, li, divHeader, divContent, divFooter :: TL.Text -> TL.Text
html x = "<!doctype html><html>" <> x <> "</html>"
head' x = "<head>" <> x <> "</head>"
title x = "<title>" <> x <> "</title>"
styleSheet x = "<link rel=\"stylesheet\" type=\"text/css\" href=\"" <> x <> "\">"
script x = "<script src=\"" <> x <> "\"></script>"
body x = "<body>" <> x <> "</body>"
wrapper x = "<div id=\"wrapper\">" <> x <> "</div>"
ul x = "<ul>" <> x <> "</ul>"
li x = "<li>" <> x
anchor s x = "<a href=\"" <> s <> "\">" <> x <> "</a>"
divHeader x = "<div id=\"header\">" <> x <> "</div>"
divContent x = "<div id=\"content\">" <> x <> "</div>"
divFooter x = "<div id=\"footer\">" <> x <> "</div>"
