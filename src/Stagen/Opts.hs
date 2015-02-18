module Stagen.Opts where

import qualified Data.Text.Lazy as TL
import Data.Default
import Data.Foldable (asum)
import Options.Applicative
import Options.Applicative.Builder.Internal (HasName)

type TargetDirectory = FilePath

data Command
    = Init
    | Build
    | Clean
    deriving (Show, Eq)

data Opts = Opts {
    optsCommand :: Command,
    optsHeader :: Maybe FilePath,
    optsFooter :: Maybe FilePath,
    optsArchive :: Maybe FilePath,
    optsStyleSheets :: [FilePath],
    optsScripts :: [FilePath],
    optsCores :: Int,
    optsTargetDirectory :: TargetDirectory
} deriving Show

optsP :: Parser Opts
optsP =
    let choice a c = command a (info (cmdOptsP c) (progDesc a))
        choices = foldr1 (<>) [choice "init" Init, choice "build" Build, choice "clean" Clean]
    in subparser choices

cmdOptsP :: Command -> Parser Opts
cmdOptsP cmd = Opts cmd
    <$> tryStrArg 'e' "header" "Include header"
    <*> tryStrArg 'f' "footer" "Include footer"
    <*> tryStrArg 'a' "archive" "Try to include archive, then generate page"
    <*> many (strArg 'c' "stylesheet" "Stylesheet file path")
    <*> many (strArg 's' "script" "Script file path")
    <*> pure (optsCores def)
    <*> targetDirectory

arg :: HasName f => Char -> String -> String -> Mod f a
arg s l h = short s <> long l <> help h

strArg :: Char -> String -> String -> Parser String
strArg c l i = strOption (arg c l i)

tryStrArg :: Char -> String -> String -> Parser (Maybe String)
tryStrArg c l i =  fmap Just (strArg c l i) <|> pure Nothing

targetDirectory :: Parser TargetDirectory
targetDirectory = strArgument (metavar "TARGET_DIRECTORY") <|> pure (optsTargetDirectory def)

instance Default Opts where
    def = Opts Build Nothing Nothing Nothing [] [] 1 "."
