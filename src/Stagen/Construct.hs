module Stagen.Construct where

import qualified Data.Text.Lazy as TL
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Stagen.Template
import Stagen.Page

construct :: Template -> Page -> TL.Text
construct Template{..} Page{..} = (html . TL.concat)
    [ (head' . TL.concat) (title pageTitle : map styleSheet tplStyleSheets ++ map script tplScripts)
    , (body . wrapper . TL.concat)
        [ divHeader (try tplHeader)
        , divContent pageContent
        , divFooter (try tplFooter) ] ]
 where
    try = fromMaybe TL.empty

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
