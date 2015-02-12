import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.List as L
import Options.Applicative
import System.FilePath.Find
import System.FilePath.Glob
import System.FilePath.Manip
import Text.Markdown
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Default
import Data.Monoid
import System.Environment
import Data.Maybe

data Config = Config {
    cfgHeader :: Bool,
    cfgFooter :: Bool,
    cfgArchive :: Bool,
    cfgStyleSheets :: [FilePath],
    cfgScripts :: [FilePath],
    cfgTargetDirectory :: FilePath
} deriving Show

data Page = Page {
    pageTitle :: Maybe TL.Text,
    pageContent :: TL.Text
} deriving Show

data Template = Template {
    tplStyleSheets :: [TL.Text],
    tplScripts :: [TL.Text],
    tplHeader :: Maybe TL.Text,
    tplFooter :: Maybe TL.Text
} deriving Show

main :: IO ()
main = do
    cfg <- execParser (info cfgP idm)
    tpl <- mkTemplate cfg
    page <- mkPage "index.md"
    TL.writeFile "index.html" (construct tpl page)
    
cfgP :: Parser Config
cfgP = Config
    <$> switch (long "header" <> help "Include header.md")
    <*> switch (long "footer" <> help "Include footer.md")
    <*> switch (long "archive" <> help "Generate archive.html page")
    <*> pure (cfgStyleSheets def)
    <*> pure (cfgScripts def)
    <*> pure (cfgTargetDirectory def)

mkTemplate :: Config -> IO Template
mkTemplate Config{..} = do
    let tplStyleSheets = []
    let tplScripts = []
    tplHeader <- go cfgHeader (render <$> TL.readFile "header.md")
    tplFooter <- go cfgFooter (render <$> TL.readFile "footer.md")
    return Template{..}
 where
    go False _ = return Nothing
    go True x = fmap Just x

mkPage :: FilePath -> IO Page
mkPage filePath = do
    let pageTitle = Nothing
    pageContent <- render <$> TL.readFile filePath
    return Page{..}

run :: FilePath -> FilePath -> IO ()
run inputFilePath outputFilePath = do
    fileContent <- TL.readFile inputFilePath
    TL.writeFile outputFilePath (translate fileContent)

render :: TL.Text -> TL.Text
render = renderHtml . markdown def

translate :: TL.Text -> TL.Text
translate content = construct (Template [] [] Nothing Nothing) (Page Nothing (render content))

construct :: Template -> Page -> TL.Text
construct Template{..} Page{..} = (html . TL.concat)
    [ (head' . TL.concat) (try pageTitle : map styleSheet tplStyleSheets ++ map script tplScripts)
    , (body . wrapper . TL.concat) [divHeader (try tplHeader), divContent pageContent, divFooter (try tplFooter)] ]
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

instance Default Config where
    def = Config False False False [] [] "."
