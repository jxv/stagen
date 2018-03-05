module Stagen.Page where

import qualified Data.Text.Lazy as TL
import Data.Default
import Stagen.Date

data Page = Page {
    pageAbsoluteUrl :: TL.Text,
    pageDate :: Maybe Date,
    pageTitle :: TL.Text,
    pageContent :: TL.Text
} deriving Show

instance Default Page where
    def = Page TL.empty Nothing TL.empty TL.empty
