module Stagen.Build where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.List as L
import Control.Arrow
import Control.Monad (void)
import System.FilePath.Find
import Data.Monoid
import Data.Maybe
import Lucid

import Stagen.Opts
import Stagen.Date
import Stagen.Template
import Stagen.Page
import Stagen.Job
import Stagen.AtomFeed (createAtomFeed)
import Stagen.RssFeed (createRssFeed)
import Stagen.File

runBuild :: Opts -> IO ()
runBuild opts@Opts{..} = do
    tpl <- mkTemplate opts
    let ignore = optsIgnore ++ catMaybes [optsHeader, optsFooter, optsArchive]
    files <- find (pure True) (eligable ignore) optsTargetDirectory
    htmlPathAndPages <- sequence $ map (fromMarkdown optsVerbose optsBaseUrl) files
    let archive = archiveJob optsVerbose tpl optsBaseUrl htmlPathAndPages optsArchive
    let pageAndHtmlPaths = map (\(page, path) -> (path, page)) htmlPathAndPages
    let atomFeed = fromMaybe (return ()) $ fmap writeAtomXml $ createAtomFeed optsTitle optsBaseUrl (reverse $ map snd htmlPathAndPages)
    let rssFeed = fromMaybe (return ()) $ fmap writeRssXml $ createRssFeed optsTitle optsBaseUrl (reverse $ map snd htmlPathAndPages)
    let jobs = archive : atomFeed : rssFeed : map (uncurry (writePage tpl)) pageAndHtmlPaths
    void $ runJobs optsJobs jobs

build :: Template -> Page -> TL.Text
build Template{..} Page{..} = renderText $ doctypehtml_ $ do
    head_ $ do
        void $ title_ (toHtmlRaw pageTitle)
        mapM_ (\href -> link_ [rel_ "stylesheet", type_ "text/css", href_ href]) (map TL.toStrict tplStyleSheets)
        mapM_ (\src -> termWith "script" [src_ src] "") (map TL.toStrict tplScripts)
        mapM_ (\href -> link_ [rel_ "icon", type_ "image/png", href_ href]) tplFavicon
    body_ . div_ [id_ "wrapper"] $ do
        div_ [id_ "header"] (try tplHeader)
        div_ [id_ "content"] (toHtmlRaw pageContent)
        div_ [id_ "footer"] (try tplFooter)
 where
    try = toHtmlRaw . fromMaybe TL.empty

archiveJob :: Verbose -> Template -> FilePath -> [(FilePath, Page)] -> Maybe FilePath -> IO ()
archiveJob verbose tpl baseUrl htmlPathAndPages mMdPath = fromMaybe (return ()) $ do
    mdPath <- mMdPath
    return $ do
        (htmlPath, page) <- fromMarkdown verbose baseUrl mdPath
        writePage tpl (addArchiveEntries baseUrl mdPath page htmlPathAndPages) htmlPath

writePageFromMarkdown :: Verbose -> Template -> FilePath -> FilePath -> IO ()
writePageFromMarkdown verbose tpl baseUrl mdPath = do
    (htmlPath, page) <- fromMarkdown verbose baseUrl mdPath
    writePage tpl page htmlPath

writePage :: Template -> Page -> FilePath -> IO ()
writePage tpl page htmlPath = TL.writeFile htmlPath (build tpl page)

writeAtomXml :: TL.Text -> IO ()
writeAtomXml = TL.writeFile "atom.xml"

writeRssXml :: TL.Text -> IO ()
writeRssXml = TL.writeFile "rss.xml"

addArchiveEntries :: FilePath -> FilePath -> Page -> [(FilePath, Page)] -> Page
addArchiveEntries baseUrl archivePath page htmlPathAndPages =
    let pathAndTitles = map (second pageTitle) htmlPathAndPages
        sorter = L.sortBy (\(_,_,a) (_,_,b)-> compare b a)
        content = (pageContent page) <> (toLinks . sorter . getEntries) pathAndTitles
    in Page{pageTitle = pageTitle page, pageContent = content, pageDate = Nothing, pageAbsoluteUrl = absoluteUrl baseUrl archivePath }
 where
    getEntries :: [(FilePath, TL.Text)] -> [(FilePath, TL.Text, Date)]
    getEntries = catMaybes . map (\(f,t) -> fmap (f,t,) (parseMay datePrefix (baseName f)))

    toLinks :: [(FilePath, TL.Text, Date)] -> TL.Text
    toLinks = renderText . ul_ . toHtmlRaw . TL.concat . map toLink

    toLink :: (FilePath, TL.Text, Date) -> TL.Text
    toLink (path, title, date) =
        renderText
            (li_ (a_ [href_ (T.pack path)]
            (toHtmlRaw $ displayDate date <> " <span id=\"title\">" <> title <> "</span>")))

displayDate :: Date -> TL.Text
displayDate Date{..} = TL.concat
    [ TL.pack (show dateYear)
    , "-"
    , TL.pack (showTwoDigits dateMonth)
    , "-"
    , TL.pack (showTwoDigits dateDay)
    ]

mkTemplate :: Opts -> IO Template
mkTemplate Opts{..} = do
    let tplStyleSheets = map TL.pack optsStyleSheets
    let tplScripts = map TL.pack optsScripts
    let tplFavicon = T.pack <$> optsFavicon
    tplHeader <- go optsHeader
    tplFooter <- go optsFooter
    return Template{..}
 where
    go Nothing = return Nothing
    go (Just path) = Just <$> render <$> TL.readFile path
