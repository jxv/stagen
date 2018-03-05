module Stagen.Opts where

import Data.Default
import Data.Bool (bool)
import Data.Monoid (mconcat, (<>))
import Options.Applicative
import Options.Applicative.Builder.Internal (HasName)

type TargetDirectory = FilePath

data Verbose = Verbose | Slient
    deriving (Show, Eq)

data Command = Init | Build | Clean
    deriving (Show, Eq)

data Opts = Opts {
    optsCommand :: Command,
    optsHeader :: Maybe FilePath,
    optsFooter :: Maybe FilePath,
    optsArchive :: Maybe FilePath,
    optsFavicon :: Maybe FilePath,
    optsStyleSheets :: [FilePath],
    optsScripts :: [FilePath],
    optsIgnore :: [FilePath],
    optsJobs :: Int,
    optsVerbose :: Verbose,
    optsBaseUrl :: FilePath,
    optsTitle :: String,
    optsTargetDirectory :: TargetDirectory
} deriving Show

parseOpts :: IO Opts
parseOpts = execParser (info optsP idm)

optsP :: Parser Opts
optsP =
    let choice a c = command a (info (cmdOptsP c) (progDesc a))
        choices = mconcat [choice "init" Init, choice "build" Build, choice "clean" Clean]
    in subparser choices

cmdOptsP :: Command -> Parser Opts
cmdOptsP cmd = Opts cmd
    <$> tryStrArg 'e' "header" "Include header"
    <*> tryStrArg 'f' "footer" "Include footer"
    <*> tryStrArg 'a' "archive" "Prepend archive to generated page"
    <*> tryStrArg 'v' "favicon" "favicon path"
    <*> many (strArg 'c' "stylesheet" "Stylesheet file path")
    <*> many (strArg 's' "script" "Script file path")
    <*> many (strArg 'i' "ignore" "Don't render this file")
    <*> (option auto (arg 'j' "jobs" "Run ARG jobs simultaneously") <|> pure (optsJobs def))
    <*> fmap (bool Slient Verbose) (switch (arg 'v' "verbose" "Explain what is being done"))
    <*> strArg 'u' "url" "Base url for the generated pages"
    <*> strArg 't' "title" "Title of the blog"
    <*> targetDirectory

arg :: HasName f => Char -> String -> String -> Mod f a
arg s l h = short s <> long l <> help h

strArg :: Char -> String -> String -> Parser String
strArg c l i = strOption (arg c l i)

tryStrArg :: Char -> String -> String -> Parser (Maybe String)
tryStrArg c l i = fmap Just (strArg c l i) <|> pure Nothing

targetDirectory :: Parser TargetDirectory
targetDirectory = strArgument (metavar "TARGET_DIRECTORY") <|> pure (optsTargetDirectory def)

instance Default Opts where
    def = Opts Build Nothing Nothing Nothing Nothing [] [] [] 1 Slient "" "" "."
