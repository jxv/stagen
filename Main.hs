{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import Control.Applicative
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

data Page = Page
    { pageTitle :: Maybe TL.Text
    , pageContent :: TL.Text
    } deriving (Show, Eq)

data Generator a = Generator
    { genWrapper :: TL.Text -> TL.Text
    , genStyleSheets :: [TL.Text]
    , genScripts :: [TL.Text]
    , genHeader :: TL.Text
    , genFooter :: TL.Text
    , genValue :: a
    }

instance Functor Generator where
    fmap f gen@Generator{..} = gen { genValue = f genValue }

instance Applicative Generator where
    pure a = Generator id [] [] TL.empty TL.empty a
    genFunc <*> gen = gen { genValue = (genValue genFunc) (genValue gen) }

instance Monad Generator where
    Generator{..} >>= f = f genValue
    return = pure

run :: FilePath -> FilePath -> IO ()
run inputFile outputFile = do
    fileContent <- TL.readFile inputFile
    TL.writeFile outputFile (translate fileContent)

translate :: TL.Text -> TL.Text
translate content = begin <> (renderHtml . markdown def $ content) <> end
 where
    begin = "<!doctype html><html><head><title></title></head><body><div id=\"wrapper\"><div id=\"header\"></div><div id=\"content\">"
    end = "</div><div id=\"footer\"></div></div></body></html>"    
