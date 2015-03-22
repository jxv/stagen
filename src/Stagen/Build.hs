module Stagen.Build where

import qualified Data.Text as T
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
import Lucid

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
build Template{..} Page{..} = renderText $ doctypehtml_ $ do
    head_ $ do
        void $ title_ (toHtmlRaw pageTitle)
        mapM_ (\href -> link_ [rel_ "stylesheet", type_ "text/css", href_ href]) (map TL.toStrict tplStyleSheets)
        mapM_ (\src -> termWith "script" [src_ src] "") (map TL.toStrict tplScripts)
    body_ . div_ [id_ "wrapper"] $ do
        div_ [id_ "header"] (try tplHeader)
        div_ [id_ "content"] (toHtmlRaw pageContent)
        div_ [id_ "footer"] (try tplFooter)
 where
    try = toHtmlRaw . fromMaybe TL.empty

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
    toLinks = renderText . ul_ . toHtmlRaw . TL.concat . map toLink

    toLink :: (FilePath, TL.Text, Date) -> TL.Text
    toLink (path, title, date) =
        renderText
            (li_ (a_ [href_ (T.pack path)]
            (toHtmlRaw $ displayDate date <> " &#8212; <i>" <> title <> "</i>")))

displayDate :: Date -> TL.Text
displayDate Date{..} = TL.concat 
    [ TL.pack (show dateYear)
    , "-"
    , TL.pack (showTwoDigits dateMonth)
    , "-"
    , TL.pack (showTwoDigits dateDay)
    ]
 where
    showTwoDigits n
        | n < 10 = '0' : show n
        | otherwise = show n

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
