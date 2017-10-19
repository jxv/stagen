module Stagen.Main where

import Stagen.Opts
import Stagen.Init
import Stagen.Build
import Stagen.Clean

main :: IO ()
main = do
    opts <- parseOpts
    runByCommand (optsCommand opts) opts

runByCommand :: Command -> Opts -> IO ()
runByCommand = \case Init -> runInit; Build -> runBuild; Clean -> runClean
