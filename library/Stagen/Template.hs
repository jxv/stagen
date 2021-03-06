module Stagen.Template where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Default

data Template = Template {
    tplStyleSheets :: [TL.Text],
    tplScripts :: [TL.Text],
    tplFavicon :: Maybe T.Text,
    tplHeader :: Maybe TL.Text,
    tplFooter :: Maybe TL.Text
} deriving Show

instance Default Template where
    def = Template [] [] Nothing Nothing Nothing
