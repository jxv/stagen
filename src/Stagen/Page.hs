module Stagen.Page where

import qualified Data.Text.Lazy as TL
import Data.Default
import Data.Maybe

data Page = Page {
    pageTitle :: Maybe TL.Text,
    pageContent :: TL.Text
} deriving Show

instance Default Page where
    def = Page Nothing TL.empty
