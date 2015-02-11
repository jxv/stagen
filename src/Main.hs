{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import Prelude hiding (head)
import Control.Applicative
import Text.Markdown
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Default
import Data.Monoid
import System.Environment
import Data.Maybe

main :: IO ()
main = do
    fileBaseName:_ <- getArgs
    run (fileBaseName ++ ".md") (fileBaseName ++ ".html")

data Page = Page
    { pageTitle :: Maybe TL.Text
    , pageContent :: TL.Text
    } deriving (Show, Eq)

data Template = Template
    { tplStyleSheets :: [TL.Text]
    , tplScripts :: [TL.Text]
    , tplHeader :: Maybe TL.Text
    , tplFooter :: Maybe TL.Text
    } deriving (Show, Eq)

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
    [ (head . TL.concat) (try pageTitle : map styleSheet tplStyleSheets ++ map script tplScripts)
    , (body . wrapper . TL.concat) [header (try tplHeader), content pageContent, footer (try tplFooter)]
    ]
 where
    try = fromMaybe TL.empty

html, head, title, styleSheet, script, body, wrapper, header, footer :: TL.Text -> TL.Text
html x = "<!doctype html><html>" <> x <> "</html>"
head x = "<head>" <> x <> "</head>"
title x = "<title>" <> x <> "</title>"
styleSheet x = "<link rel=\"stylesheet\" type=\"text/css\" href=\"" <> x <> "\">"
script x = "<script src=\"" <> x <> "\"></script>"
body x = "<body>" <> x <> "</body>"
wrapper x = "<div id=\"wrapper\">" <> x <> "</div>"
header x = "<div id=\"header\">" <> x <> "</div>"
content x = "<div id=\"content\">" <> x <> "</div>"
footer x = "<div id=\"footer\">" <> x <> "</div>"
