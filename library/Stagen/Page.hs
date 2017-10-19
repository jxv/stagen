module Stagen.Page where

import qualified Data.Text.Lazy as TL
import Data.Default

data Page = Page {
    pageTitle :: TL.Text,
    pageContent :: TL.Text
} deriving Show

instance Default Page where
    def = Page TL.empty TL.empty
