module Stagen.Clean where

import Control.Applicative (pure, (<$>))
import Data.Maybe (catMaybes)
import System.FilePath.Find (find)
import System.IO.Error (catchIOError)
import System.Directory (removeFile)

import Stagen.Opts
import Stagen.File

runClean :: Opts -> IO ()
runClean Opts{..} = do
    let ignore = optsIgnore ++ catMaybes [optsHeader, optsFooter]
    files <- find (pure True) (eligable ignore) optsTargetDirectory
    htmlPaths <- map fst <$> (sequence $ map (fromMarkdown optsVerbose optsBaseUrl) files)
    mapM_ (\p -> catchIOError (removeFile p) (const done)) htmlPaths
 where
    done = return ()
