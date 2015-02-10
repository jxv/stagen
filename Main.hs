{-# LANGUAGE OverloadedStrings #-}
import Text.Markdown
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Default
import Data.Monoid
import System.Environment

main :: IO ()
main = do
    fileBaseName:_ <- getArgs
    run (fileBaseName ++ ".md") (fileBaseName ++ ".html")

run :: FilePath -> FilePath -> IO ()
run inputFile outputFile = do
    fileContent <- TL.readFile inputFile
    TL.writeFile outputFile (translate fileContent)

translate :: TL.Text -> TL.Text
translate content = begin <> (renderHtml . markdown def $ content) <> end
 where
    begin = "<!doctype html><html><body>"
    end = "</body></html>"    
