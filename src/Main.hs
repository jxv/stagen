module Main where

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
import Text.Markdown
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Default
import Data.Monoid
import Data.Maybe
import Control.Parallel.Strategies

import Stagen.Opts
import Stagen.Page
import Stagen.Template
import Stagen.Construct

data Date = Date {
    dateYear :: Int,
    dateMonth :: Int,
    dateDay :: Int
} deriving (Show, Eq, Ord)

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
    let ignore = optsIgnore ++ catMaybes [optsHeader, optsFooter, optsArchive]
    files <- find (pure True) (eligable ignore) optsTargetDirectory
    htmlPathAndPages <- sequence $ map (fromMarkdown optsVerbose) files
    let archive = archiveJob optsVerbose tpl htmlPathAndPages optsArchive
    let pageAndHtmlPaths = map (\(page, path) -> (path, page)) htmlPathAndPages
    let jobs = archive : map (uncurry (writePage tpl)) pageAndHtmlPaths
    void $ runJobs optsCores jobs

runJobs :: Monad m => Int -> [m a] -> m [a]
runJobs n = sequence . runEval . evalBuffer n rseq

changeExtension :: FilePath -> String -> FilePath
changeExtension path newExtension
    | hasExt = basename ++ newExtension
    | otherwise = path ++ ('.' : newExtension)
 where
    revPath = reverse path
    hasExt = elem '.' (takeWhile (/= '/') revPath)
    basename = reverse (dropWhile (/= '.') revPath)

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

fromMarkdown :: Verbose -> FilePath -> IO (FilePath, Page)
fromMarkdown verbose mdPath = do
    let htmlPath = changeExtension mdPath "html"
    page <- readPage mdPath
    when (verbose == Verbose) (putStrLn htmlPath)
    return (htmlPath, page)

writePage :: Template -> Page -> FilePath -> IO ()
writePage tpl page htmlPath = TL.writeFile htmlPath (construct tpl page)

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

datePrefix :: P.Parser Date
datePrefix = do
    year <- number
    void $ P.char '-'
    month <- number
    void $ P.char '-'
    day <- number
    void $ P.char '-'
    return (Date year month day)
 where
    number = fmap read (P.many1 P.digit)

parseMay :: P.Parser a -> String -> Maybe a
parseMay p src = case P.parse p "" src of
    Left _ -> Nothing
    Right x -> Just x

eligable :: [FilePath] -> FindClause Bool
eligable ignore = do
    isMarkdown <- (== ".md") <$> extension
    name <- fileName
    let isFileName = and (map (/= name) ignore)
    return (isMarkdown && isFileName) 

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

readPage :: FilePath -> IO Page
readPage filePath = do
    content <- TL.readFile filePath
    let pageTitle = TL.filter (not . isMarkdownChar) (TL.takeWhile isNotNewLine content)
    let pageContent = render content
    return Page{..}
 where
    isMarkdownChar ch = ch == '_' || ch == '*' || ch == '#' || ch == '>'
    isNotNewLine ch = ch /= '\n' && ch /= '\r'

render :: TL.Text -> TL.Text
render = renderHtml . markdown def
